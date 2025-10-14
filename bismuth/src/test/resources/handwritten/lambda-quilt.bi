(256, 256);

y' = y * 1.1;
x' = x * 2;
lambda = (-1) < x' && x' < 1 && (-1) < y' && y' < 1 && (x' - y' < 0.1 && y' - x' < 0.1 || x' + y' < 0.1 && x' + y' > -0.1) && ! (x' > 0 && y' > 0 && x' + y' > 0.1);

lambda' = translate(-0.5, 0.5, scale(0.5, rotate(-pi/10, swirl(pi / 10, lambda))));
lambdaOp = lambda' || rotate(pi, lambda');
lambdaOp || rotate(pi / 2, lambdaOp)
