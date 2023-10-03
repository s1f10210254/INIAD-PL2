int foo(int a, int b)
{
    int tmp1 = a - 1;
    int tmp2 = abs(tmp1);
    int tmp3 = b - 1;
    int tmp4 = abs(tmp3);
    int tmp5 = tmp2 + tmp4;
    return tmp5;
}

// レジスタ割り当て
int foo(int %edi, int %esi)
{
    int %edi = %edi - 1;          ; %edi レジスタの値を1減算して tmp1 変数に代入
    Save(b, %esi);                ; %esi レジスタの値をスタックに保存
    int %eax = abs(%edi);         ; %edi レジスタの絶対値を %eax レジスタに代入
    Restore(b, %edi);             ; %esi レジスタの値をスタックから復元
    int %edi = %edi - 1;          ; %edi レジスタの値を再度1減算して tmp3 変数に代入
    Save(tmp2, %eax);             ; %eax レジスタの値をスタックに保存
    int %eax = abs(%edi);         ; %edi レジスタの絶対値を再度 %eax レジスタに代入
    Restore(tmp2, %X);            ; %eax レジスタの値をスタックから復元
    int %eax = %eax + %X;         ; tmp2 と tmp4 の値を足して %eax レジスタに代入
    return %eax;                  ; %eax レジスタの値を返り値として返す
}

int baz(int a, int b){
    int tmp1 = a * a;
    int tmp2 = tmp1 - b;
    int tmp3 = abs(tmp2);
    int tmp4 = tmp3 + a;
    return tmp4;
}
// レジスタ割り当て
int baz(int %edi, int %esi)
{
    Save(a, %edi);
    int %edi = %edi * %edi;
    int %edi = %edi - %esi;
    int %eax = abs(%edi);
    Restore(a, %edx);
    int %eax = %eax + %edx;
    return %eax;
}

int baz(int a, int b, int c)
{
    int tmp1 = a * b;
    int tmp2 = tmp1 * c;
    int tmp3 = abs(tmp2);
    return tmp3;
}

int baz(int %edi, int %esi, int %edx)
{
    int %edi = %edi * %esi;
    int %edi = %edi * %edx;
    int %eax = abs(%edi);
    return %eax;
}

int sin2(int x)
{
    int tmp1 = sin(x);
    int tmp2 = 2 * tmp1;
    int tmp3 = cos(x);
    int tmp4 = tmp2 * tmp3;
    return tmp4;
}


int sin2(int %edi)
{
    Save(x, %edi)
    %eax = sin(%edi); 
    %esi = 2 * %eax;  //tmp2
    Restore(x,%edi);  
    %edi = cos(%edi); //tmp3
    %eax = %esi * %edi;
    return %eax;


}

int fmasqrt(int a, int b, int c)
{
    int tmp1 = b * c;
    int tmp2 = a + tmp1;
    int tmp3 = sqrt(tmp2);
    return tmp3;
}
int fmasqrt(int %edi, int %esi, int %edx)
{
    int %esi = %esi * %edx;
    int %edx = %edi + %esi;
    int %eax = sqrt(%edx);
    return %eax;
}