import java.lang.classfile.ClassFile;
import java.lang.constant.*;
import java.nio.file.*;
import static java.lang.constant.ConstantDescs.*;

/**
 * Generates CondyExample.class, a class whose constant pool contains CONSTANT_Dynamic
 * (tag 17) entries loaded with ldc / ldc2_w. javac never emits CONSTANT_Dynamic, so the
 * class is assembled with the Class-File API (java.lang.classfile, final since JDK 24).
 *
 * Usage: java CondyGen <output .class path>   (see java-assets/compile.sh)
 */
public class CondyGen {
    public static void main(String[] args) throws Exception {
        ClassDesc thisClass = ClassDesc.of("CondyExample");
        ClassDesc printStream = ClassDesc.of("java.io.PrintStream");
        ClassDesc system = ClassDesc.of("java.lang.System");

        // ldc of a reference-typed condy (ConstantBootstraps.nullConstant)
        ConstantDesc nullCondy = ConstantDescs.NULL;
        // ldc of a Class-typed condy (ConstantBootstraps.primitiveClass)
        DynamicConstantDesc<Class<?>> intClass = DynamicConstantDesc.ofNamed(BSM_PRIMITIVE_CLASS, "I", CD_Class);
        // ldc2_w of a long-typed condy (ConstantBootstraps.explicitCast with a static Integer arg)
        DynamicConstantDesc<Long> longCondy = DynamicConstantDesc.ofNamed(BSM_EXPLICIT_CAST, "_", CD_long, 42);

        byte[] bytes = ClassFile.of().build(thisClass, cb -> {
            cb.withVersion(ClassFile.JAVA_11_VERSION, 0);
            cb.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_SUPER);
            cb.withMethodBody("getNull", MethodTypeDesc.of(CD_Object),
                ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, code -> code.ldc(nullCondy).areturn());
            cb.withMethodBody("getIntClass", MethodTypeDesc.of(CD_Class),
                ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, code -> code.ldc(intClass).areturn());
            cb.withMethodBody("getLong", MethodTypeDesc.of(CD_long),
                ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, code -> code.ldc(longCondy).lreturn());
            cb.withMethodBody("main", MethodTypeDesc.of(CD_void, CD_String.arrayType()),
                ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, code -> code
                    .getstatic(system, "out", printStream)
                    .invokestatic(thisClass, "getIntClass", MethodTypeDesc.of(CD_Class))
                    .invokevirtual(printStream, "println", MethodTypeDesc.of(CD_void, CD_Object))
                    .getstatic(system, "out", printStream)
                    .invokestatic(thisClass, "getLong", MethodTypeDesc.of(CD_long))
                    .invokevirtual(printStream, "println", MethodTypeDesc.of(CD_void, CD_long))
                    .getstatic(system, "out", printStream)
                    .invokestatic(thisClass, "getNull", MethodTypeDesc.of(CD_Object))
                    .invokevirtual(printStream, "println", MethodTypeDesc.of(CD_void, CD_Object))
                    .return_());
        });
        Files.write(Path.of(args[0]), bytes);
    }
}
