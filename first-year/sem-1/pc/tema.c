#include<stdio.h>
int main(void)
{
int u,n,p=1,nr=0;
printf("da nr copile \n");
scanf("%d",&n);
while(n!=0)
{
u=n%16;
switch(u)
{
case 1:
case 2:
case 3:
case 4:
case 5:
case 6:
case 7:
case 8:
case 9:
{
printf("%d",u);
break;
}
case 10:
{
printf("A");
break;
}
case 11:
{
printf("B");
break;
}
case 12:
{
printf("C");
break;
}
case 13:
{
printf("D");
break;
}
case 14:
{
printf("E");
break;
}
case 15:
{
printf("F");
break;
}
}
n=n/16;
}



return 0;

}
