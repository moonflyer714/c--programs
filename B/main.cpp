#include <cstdio>
#include <cstdlib>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cstring>
#include <string>
#include <cmath>
#include <vector>
#include <queue>
#include <stack>
#include <set>
#include <map>
#define  eps 1e-7
using namespace std;
int n,m;
double x[410][20],b,c,a[410],u[410];
int y[410];
double kernel(double x1[],double x2[])
{
    double ans=0;
    for(int i=0;i<m;i++) ans+=x1[i]*x2[i];
    return ans;
}
int takeStep(int i1,int i2)
{
    if(i1==i2) return 0;
    double E1=u[i1]-y[i1],E2=u[i2]-y[i2];
    double l,h;
    if(y[i1]*y[i2]>0) l=max(0.0,a[i1]+a[i2]-c),h=min(c,a[i1]+a[i2]);
    else l=max(a[i2]-a[i1],0.0),h=min(c,c+a[i2]-a[i1]);
    if(abs(l-h)<eps) return 0;
    double k11=kernel(x[i1],x[i1]);
    double k12=kernel(x[i1],x[i2]);
    double k22=kernel(x[i2],x[i2]);
    double eta=k11+k22-2*k12;
    double a1,a2;
    if(eta>eps)
    {
        a2=a[i2]+y[i2]*(E1-E2)/eta;
        if(a2<l) a2=l;
        else if(a2>h) a2=h;
    }
    else
    {
        double lm=eta*l*l+2*y[i2]*(E2-E1-y[i2]*a[i2]*eta)*l;
        double hm=eta*h*h+2*y[i2]*(E2-E1-y[i2]*a[i2]*eta)*h;
        if(lm<hm-eps) a2=l;
        else if(lm>hm+eps) a2=h;
        else a2=a[i2];
    }
    if(abs(a2-a[i2])<eps) return 0;
    a1=a[i1]+(a[i2]-a2)*y[i1]*y[i2];
    double b1=E1+y[i1]*(a1-a[i1])*k11+y[i2]*(a2-a[i2])*k12;
    double b2=E2+y[i1]*(a1-a[i1])*k12+y[i2]*(a2-a[i2])*k22;
    if(a1>eps&&a1<c-eps) b=b1;
    else if(a2>eps&&a2<c-eps) b=b2;
    else b=(b1+b2)/2;
    for(int i=0;i<n;i++)
    {
        u[i]+=(a2-a[i2])*y[i2]*kernel(x[i2],x[i]);
        u[i]+=(a1-a[i1])*y[i1]*kernel(x[i1],x[i]);
    }
    a[i1]=a1,a[i2]=a2;
    return 1;
}
int examineExample(int i2)
{
    double E2=u[i2]-y[i2];
    double r2=(E2-b)*y[i2];
    if((r2<-eps&&a[i2]<c-eps)||(r2>eps&&a[i2]>eps))
    {
        if(a[i2]>eps&&a[i2]<c-eps)
        {
            double E;
            int i1=-1;
            for(int i=0;i<n;i++)
            {
                if(i==i2) continue;
                double E1=u[i]-y[i];
                if(i1==-1||abs(E1-E2)>abs(E-E2)) E=E1,i1=i;
            }
            if(i1>=0&&takeStep(i1,i2)) return 1;
        }
        for(int i=0,j=rand()%n;i<n;i++,j++)
        {
            if(j==n) j=0;
            if(j==i2) continue;
            if(a[j]>eps&&a[j]<c-eps&&takeStep(j,i2)) return 1;
        }
        for(int i=0,j=rand()%n;i<n;i++,j++)
        {
            if(j==n) j=0;
            if(j==i2) continue;
            if(takeStep(j,i2)) return 1;
        }
    }
    return 0;
}
void SMO()
{
    int num=1;
    for(int i=0;i<n;i++) a[i]=0.0,u[i]=0.0;
    b=0.0;
    int numChanged=0,examineAll=1;
    while(numChanged|examineAll)
    {
        num++;
        printf("%d\n",num);
        numChanged=0;
        if(examineAll)
            for(int i=0;i<n;i++) numChanged+=examineExample(i);
        else
        {
            for(int i=0;i<n;i++)
                if(a[i]>eps&&a[i]<c-eps) numChanged+=examineExample(i);
        }
        if(examineAll) examineAll=0;
        else if(numChanged==0) examineAll=1;
    }
    return;
}
int main()
{
    freopen("in.txt","r",stdin);
    scanf("%d%d",&n,&m);
    for(int i=0;i<n;i++)
    {
        for(int j=0;j<m;j++) scanf("%lf",&x[i][j]);
        scanf("%d",&y[i]);
        if(y[i]==2) y[i]=-1;
    }
    //scanf("%lf",&c);
    c=0.5;
    SMO();
    double w[20];
    for(int i=0;i<m;i++)
    {
        double ans=0.0;
        for(int j=0;j<n;j++) ans+=a[j]*y[j]*x[j][i];
        w[i]=ans;
        printf("%f ",ans);
    }
    printf("%f\n",b);
    int gg=0;
    while(scanf("%lf",&x[n][0])!=EOF)
    {
        for(int j=1;j<m;j++) scanf("%lf",&x[n][j]);
        double ans=0;
        for(int i=0;i<m;i++) ans+=w[i]*x[n][i];
        ans-=b;
        scanf("%d",&y[n]);
        if((y[n]==1&&ans>=0)||(y[n]!=1&&ans<0)) printf("Yes\n");
        else printf("No\n"),gg++;
    }
    printf("%d\n",gg);
    return 0;
}
/*
16 2
1 1 1
1 3 1
2 3 1
2 5 1
3 4 1
3 5 1
4 5 1
3 1 2
4 1 2
4 2 2
5 1 2
5 2 2
5 3 2
5 4 2
6 2 2
6 3 2
*/
