num = []
n=int(input())
for i in range(n):
    num.append(int(input().split()))
    
m=int(input())
test=[]
for i in range(m):
    test.append(int(input().split()))
    
for i in test:
  for j in num:
    if test[i] == num[j]:
      print(0)
    else:
      print(1)
    
    
