int foo(int n){
    int sum = 0;
    while (n > 0)
    {
        if ((n% 2) == 0){
            sum += n * n;
        }else{
            sum -= n * n;
        }
        n--;

    }
    return sum;
}


//K正規化
int foo(int n){
    int sum0 = 0;
    int sum1 = 0;
    while (n > 0)
    {
        if ((n%2) == 0){
            sum1 = sum0 + n * n;
        }
        else{
            sum1 = sum0 - n* n;
        }
        n--;
    }
    return sum1;
    
}

int bar(int x){
    int count = 0;
    while (x>0)
    {
        x /= 10;
        count ++;
    }
    return count;   
}
//SSA変換
int bar(int x0){
    int count0 = 0;
    int x1,x2,count1,count2;
    while(x2 = phi(x0,x1),count2 = (count0,count1), x2 > 0){
        x1 = x2 / 10;
        count1 = count2 + 1;
    }
    return count2;

}


int fact(int n)
{
    int result = 1;
    int i = n;

    while (i > 0) {
       result *= i;
       i -= 1;
    }

    return result;
}

//SSA変換
int fact(int n0){
    int result0 = 1;
    int i0 = n0;
    int i1,i2,result1,resutlt2;
    while (i2 = phi(i0,i1), resutlt2=phi(result0,result1), i2 > 0){
        result1 = resutlt2 * i2;
        i1 = i2 -1;
    }
    return resutlt2;
}


int sumrange(int x, int y)
{
    int result = 0;

    int i = x;
    while (i < y) {
        result += i;
        i++;
    }

    return result;
}

//SSA変換
int sumrange(int x0, int y)
{
    int result0 = 0;
    int i0 = x0;

    int i1,i2,result1,result2;
    while (i2 = phi(i0,i1), result2=phi(result0,result1), i2<y)
    {   
        result1 = result2 + i2;
        i1 = i2 + 1;
    }
    return result2;
    
}

int max(int x0, int y0)
{
    int max0 = x0;
    int max1;
    if(y0 > max0){
        max1 = y0;
    }
    return max1;
}