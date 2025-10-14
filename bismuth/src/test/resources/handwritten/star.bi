(128, 128);

leg = (theta < pi/2 || theta > (2+1/2-4/5) * pi) && y < 1 - x * (1 + sin((4/5-1/2)*pi)) / cos((4/5-1/2)*pi);

l2 = rotate(4/5*pi, leg);
l3 = rotate(4/5*pi, l2);
l4 = rotate(4/5*pi, l3);
l5 = rotate(4/5*pi, l4);

star = leg || l2 || l3 || l4 || l5;

overlay(if star { red } else { [0, 0, 0, 0] }, if r < 1 { blue } else { white })

// leg || rotate(3 * pi/5, leg)

// l || rotate(2 * pi/5, l)
