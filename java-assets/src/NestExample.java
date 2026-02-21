public class NestExample {
    private int value = 42;

    class Inner {
        int getValue() {
            return value;
        }
    }
}
