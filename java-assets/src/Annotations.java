import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class Annotations {
  @Retention(RetentionPolicy.RUNTIME)
  public @interface VisibleAtRuntime {
    String value();
  }

  @Retention(RetentionPolicy.CLASS)
  public @interface InvisibleAtRuntime {
    String value();
  }

  @VisibleAtRuntime(value = "visisble")
  @InvisibleAtRuntime(value = "invisible")
  public void myMethod(String s) {
    System.out.println(s);
  }

  public void main(String[] args) {
    Annotations annotations = new Annotations();
    annotations.myMethod("Hello, World!");
  }
}
