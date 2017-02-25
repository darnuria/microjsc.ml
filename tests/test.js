


12;
true;
false;
12 + 4;
true + 14 + (9 + false);
true + 14 * (9 + false);
true + 14 * 9 + false;

24;

if (x == true) {
    42;
} else {
    37 * 9;
};

function fact(n) {
    if (n == 0) {
        return 1;
    } else {
        return n * fact(n - 1);
    };
};

var x = 42; // portée globale

let y = 52; // portée lexicale
x + y;
// implicitement on est ici dans la portée lexicale de y

z = x * y + 9;

while (x < 50) {
    x = x + 1;
};


for (x in y) {
    z = z + x;
    x + y * 2;
};

