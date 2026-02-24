public class PrependTest {
    public static void main(String[] args) {
        // Call all methods so their descriptors are in the constant pool
        withParams("");
        withLocal();
        withTryCatch();
        withBranch(0);
    }
    public static void withParams(String name) {
        System.out.println("hello " + name);
    }
    public static void withLocal() {
        int x = 10;
        System.out.println("x=" + x);
    }
    public static void withTryCatch() {
        try {
            System.out.println("try");
        } catch (Exception e) {
            System.out.println("catch");
        }
    }
    public static void withBranch(int n) {
        System.out.println("n=" + n);
    }
}
