package bismuth
import cats.Applicative
import cats.syntax.all._

def liftA4[F[_]: Applicative, A, B, C, D, E](
    f: (A, B, C, D) => E,
    fa: F[A],
    fb: F[B],
    fc: F[C],
    fd: F[D]
): F[E] =
  (fa, fb, fc, fd).mapN(f)
