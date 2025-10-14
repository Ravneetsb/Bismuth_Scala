(128, 128);

circle = r < 1;
top = translate(0, 0.5, scale(1, 0.5, circle <..> circle));
bottom = y < (1 - 1/sqrt(2))/2 && y > x - 1/sqrt(2) && y > -x - 1/sqrt(2);
patch = scale(1/2, x < 1 && y < 1 && x > -1 && y > 0);

if scale(0.6, 0.8, top || bottom || patch) {
  red
} else {
  white
}