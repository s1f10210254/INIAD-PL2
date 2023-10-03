int foo(int x, int y)
{
    if (x * x < y){
        return y;
    }
    return x * y;
}
// K正規化
int foo(int x, int y){
    int temp1 = x * x;
    int temp2 = x * y;
    if (temp1 < y){
        return y;
    }
    return temp2;
}



int foo(int n){
    int sum = 0;
    while (n> 0){
        if ((n%2)==0){
            sum += n *n;
        }else{
            sum -= n*n;
        }
        n--;
    }
    return sum;                             
}
//k正規化
int foo(int n)
{
    int sum = 0;
    int temp1 = n % 2;
    int temp2 = n* n;
    int sum0 = 0;
    int sum1 = 0;
    while (n > 0)
    {
        if(temp1 == 0){
             sum1 = sum0 + temp2;
        }
        else{
            sum1 = sum0 - temp2;
        }
        sum0 = sum1; // sum1の値をsum0に代入する
        n--;
    }
    return sum1;
    
}


// double heron(double s, double a, double b, double c)
// {
//     return sqrt(s * (s-a) * (s-b) * (s-c));
// }

double heron(double s,double a,double b, double c){
    double tmp1 = s-a;
    double tmp2 = s-b;
    double tmp3 = s-c;
    double tmp4 = s * tmp1 * tmp2 * tmp3;
    double tmp5 = sqrt(tmp4);
    return tmp5;
}


int foo(int x, int y, int z){
    int tmp1 =  x * y;
    int tmp2 = tmp1 * z;
    return tmp2;
}