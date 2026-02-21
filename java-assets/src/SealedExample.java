public sealed class SealedExample permits SealedChild1, SealedChild2 {}

final class SealedChild1 extends SealedExample {}
final class SealedChild2 extends SealedExample {}
