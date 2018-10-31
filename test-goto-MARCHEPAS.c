{
a:a = a+1;
print(a);
do{
c:if (a == 1) {goto a;}
print(1);
goto b;
} while(1);
print(1);
b:;
if (a == 2) {
a=3;
a; goto a;
}
print(a);
}
