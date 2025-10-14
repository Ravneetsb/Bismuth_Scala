(256, 256);

y_ = y * 1.1;
x_ = x * 2;
leg' = x_ > -1 && x < 0 && y_ > -1 && (x_ + y_ < 0.1 && -0.1 < x_ + y_);
leg = rotate(-pi/10, swirl(pi / 10, leg'));
lambda = leg || flip(leg') || rotate(pi, leg);

lc = replicate(4, lambda);

{

if lc {
    red
} else {
    [1, 0.5, 0.5]
}
<:>

if lc {
    [1, 0.5, 0]
} else {
    yellow
}

} <:> {

if lc {
    [0, 0.6, 0]
} else {
    [0.5, 1, 0.5]
}

<:>

if lc {
    [0.7, 0, 0.7]
} else {
    [1, 0.5, 1]
}
    
}
