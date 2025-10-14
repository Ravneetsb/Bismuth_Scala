(950, 500);

// helpers
transparent = [0, 0, 0, 0];
inUnitBox = x > -1 && x < 1 && y > -1 && y < 1;

red' = [ 0.698, 0.132, 0.203 ];
blue' = [ 0.234, 0.233, 0.430 ];

// the stripes
redstripe = if x > 0 { red' } else { white };
stripes = translate(0, -1/13, scale(14/13, rotate(pi/2, replicate(7, redstripe))));

// circle instead of star for now
circle = r < 1;

// star
angleConstraint = (theta < pi/2 || theta > (2+1/2-4/5) * pi);
leg = angleConstraint && y < 1 - x * (1 + sin((4/5-1/2)*pi)) / cos((4/5-1/2)*pi);

l2 = rotate(4/5*pi, leg);
l3 = rotate(4/5*pi, l2);
l4 = rotate(4/5*pi, l3);
l5 = rotate(4/5*pi, l4);

star = leg || l2 || l3 || l4 || l5;

star' = scale(4/7, scale(1, (6/13)/(2/5), star));

oddLine = replicate(5, star');
oddLines = rotate(pi/2, replicate(4, rotate(-pi/2, oddLine)));
// remove repeated stars on the edges
oddLines = oddLines && inUnitBox;
oddLines = scale(5/6, 4/5, oddLines);

evenLine = replicate(6, star');
evenLines = rotate(pi/2, replicate(5, rotate(-pi/2, evenLine)));

stars = if evenLines || oddLines { white } else { transparent };

// The design of the canton
cantonDesign = overlay(stars, blue');

// make the canton design transparent if it is out of bounds, to allow overlaying
canton = if inUnitBox {
    cantonDesign
} else {
    transparent
};
// put canton in place and overlay it on top of the stripes
overlay(translate(-(1 - 2/5), (1 - 6/13), scale(2/5, 6/13, canton)), stripes)
