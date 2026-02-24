public class ParamAccess {
    public static void main(String[] args) {
        // Call other methods so their descriptors are in the constant pool
        wideParams(0, 0L, "");
        new ParamAccess().instanceMethod("");
    }
    public static void wideParams(int a, long b, String c) {
        System.out.println("original");
    }
    public void instanceMethod(String name) {
        System.out.println("original");
    }
}
