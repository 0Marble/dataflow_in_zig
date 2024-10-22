entry b1:
	(1) a = 1;
	(2) b = 2;
	goto: b2;

b2:
	(3) c = a + b;
	(4) d = c - a;
	goto: b3;

b3:
	(5) d = b + d;
	goto: b4, b5;

b4:
	(6) d = a + b;
	(7) e = e + 1;
	goto: b3;

b5:
	(8) b = a + b;
	(9) e = c - a;
	goto: b2, b6;

exit b6:
	(10) a = b * d;
	(11) b = a - d;
