entry b1: 
	goto: b2, b5;

b2: 
	(0) c = 2;
	goto: b3;

b3:
	goto: b4;

b4:
	goto: b7;

b5:
	(1) a = b + c;
	goto: b6;

b6:
	goto: b7;

b7:
	(2) d = b + c;
	goto: b8;

b8:
	goto: b9, b11;

b9:
	(3) e = b + c;
	goto: b10;

b10:
	goto: b9, b11;

exit b11:
