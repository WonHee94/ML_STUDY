import pandas as pd
from tqdm import tqdm
import matplotlib.pyplot as plt
from sklearn import linear_model

data = pd.read_csv("D:/2019-2nd-ml-month-with-kakr/train.csv")

'''
ID : 집을 구분하는 번호
date : 집을 구매한 날짜
price : 집의 가격(Target variable)
bedrooms : 침실의 수
bathrooms : 화장실의 수
sqft_living : 주거 공간의 평방 피트(면적)
sqft_lot : 부지의 평방 피트(면적)
floors : 집의 층 수
waterfront : 집의 전방에 강이 흐르는지 유무 (a.k.a. 리버뷰)
view : 집이 얼마나 좋아 보이는지의 정도
condition : 집의 전반적인 상태
grade : King County grading 시스템 기준으로 매긴 집의 등급
sqft_above : 지하실을 제외한 평방 피트(면적)
sqft_basement : 지하실의 평방 피트(면적)
yr_built : 지어진 년도
yr_renovated : 집을 재건축한 년도
zipcode : 우편번호
lat : 위도
long : 경도
sqft_living15 : 2015년 기준 주거 공간의 평방 피트(면적, 집을 재건축했다면, 변화가 있을 수 있음)
sqft_lot15 : 2015년 기준 부지의 평방 피트(면적, 집을 재건축했다면, 변화가 있을 수 있음)
'''
# 데이터 형태
print('데이터의 크기 :{}'.format(data.shape))

#결측치 확인
for i in range(len(data.columns)):
    if len(data[data.iloc[:,i].isna()==True]) > 0:
        print(data.columns[i],'의 결측치 갯수 : ', len(data[data.iloc[:,i].isna()==True]))
    else:
        print(data.columns[i],'결측치 없음')

# 기초통계
feature_names=['bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot',
       'floors', 'view', 'condition', 'grade', 'sqft_above',
       'sqft_basement', 'yr_built', 'yr_renovated', 'lat', 'long',
       'sqft_living15', 'sqft_lot15']
for i in range(len(data[feature_names].columns)):
    print(feature_names[i],'기초통계값\n{}'.format(data[feature_names[i]].describe()))
    print('\n')

# 가격별로 class나누기
bins = [0,1000001,2000001,3000001,4000001,5000001,6000001,7000001,max(data['price'])]
data['price_class']=pd.cut(data['price'], bins, labels=['0~1m','1~2m','2~3m','3~4m','4~5m','5~6m','6~7m','7m~'])

# 날짜 나누기: 년/월/일
for i in tqdm(range(len(data))):
    data.ix[i,'year'] = data.ix[i,'date'][0:4]
    data.ix[i,'month'] = data.ix[i,'date'][4:6]
    data.ix[i,'day'] = data.ix[i,'date'][6:8]
    data.ix[i,'date1'] = data.ix[i,'date'][8:]

# T000000이외의 값이 있는지 확인
data['date1'].unique()
# 없으므로 삭제
data = data.drop('date',axis=1)
data = data.drop('date1',axis=1)
data.head()

# 리모델링 여부 확인
renovated = data[data['yr_renovated']!=0]
print('리모델링한 집의 갯수 :{}'.format(len(renovated)))

# 집의 면적이 달라졌는지 확인
living_changed = data[data['sqft_living'] != data['sqft_living15']]
print('거실 면적이 달라진 집의 갯수 :{}'.format(len(living_changed)))
lot_changed = data[data['sqft_lot'] != data['sqft_lot15']]
print('전체 면적이 달라진 집의 갯수 :{}'.format(len(lot_changed)))
living_lot_changed = data[(data['sqft_living']!=data['sqft_living15'])&(data['sqft_living']!=data['sqft_living15'])]
print('거실과 전체 면적이 모두 달라진 리모델링 집의 갯수 :{}'.format(len(living_lot_changed)))

# 리모델링한집 중에서 면적이 달라진 갯수
reno_living = renovated[renovated['sqft_living']!=renovated['sqft_living15']]
print('거실 면적이 달라진 리모델링 집의 갯수 :{}'.format(len(reno_living)))
reno_lot = renovated[renovated['sqft_lot'] != renovated['sqft_lot15']]
print('전체 면적이 달라진 리모델링 집의 갯수 :{}'.format(len(reno_lot)))
reno_living_lot = renovated[(renovated['sqft_living']!=renovated['sqft_living15'])&(renovated['sqft_living']!=renovated['sqft_living15'])]
print('거실과 전체 면적이 모두 달라진 리모델링 집의 갯수 :{}'.format(len(reno_living_lot)))
# 거실면적이 달라진 집 => 전체 면적이 달라졌음

# 면적이 달리진 여부를 feature로 추가
data['sqft_living_changed'] = 0
data['sqft_lot_changed'] = 0
data.loc[living_changed.index,'sqft_living_changed'] = 1
data.loc[lot_changed.index,'sqft_lot_changed'] = 1
data.head()

# 비중 살펴보기 value_counts가 true면 비중으로 보여줌
data['grade'].value_counts(True)
data['price_class'].value_counts(True)
sum(data['price_class'].value_counts(True)[0:4])


# 그래프
plt.bar(data['grade'].unique(), data['grade'].value_counts())
plt.xlabel('Grade')
plt.ylabel('Count')
plt.show()

# 가격별 분포 그래프 / 오름차순

plt.bar(data['price_class'].unique(), data['price_class'].value_counts())
plt.xlabel('Grade')
plt.ylabel('Count')
plt.show()

# 변수별로 price와 상관관계
feature_names=['bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot',
       'floors', 'waterfront', 'view', 'condition', 'grade', 'sqft_above',
       'sqft_basement', 'yr_built', 'yr_renovated', 'zipcode', 'lat', 'long',
       'sqft_living15', 'sqft_lot15', 'year', 'month', 'day', 'sqft_changed',
       'sqft_living_changed', 'sqft_lot_changed','price_class']

plt.figure()
for i in range(len(data[feature_names].columns)):
    # yr_renovated는 0값이 리모델링하지 않은 것이므로, 리모델링한 데이터들만 사용
    if feature_names[i] == 'yr_renovated':
        plt.scatter(data[data['yr_renovated']!=0]['yr_renovated'], data[data['yr_renovated']!=0]['price'], marker='.')
    else:
        plt.scatter(data[feature_names[i]], data['price'], marker='.')
    plt.xlabel(feature_names[i])
    plt.ylabel('Price')
    plt.show()
# bedrooms, floors, waterfront, view, zipcode, date는 price와 관련이 없어보임
# bathrooms, sqft_lot, condition, sqft_basement, yr_built, lat, long, sqft_lot15, 확장여부는 관련이 있어보임
# sqrt_living, grade, sqft_above, yr_renovated, sqft_livig15는 큰 관련이 있어보임
# 관련없는 변수, 불리언변수, 명목형변수 제거하고 boxplot 그려보기
feature_names=['price','bathrooms', 'sqft_living', 'sqft_lot',
        'condition', 'grade', 'sqft_above',
       'sqft_basement', 'yr_built', 'yr_renovated','lat', 'long',
       'sqft_living15', 'sqft_lot15']
for i in range(len(data[feature_names].columns)):
    # yr_renovated는 0값이 리모델링하지 않은 것이므로, 리모델링한 데이터들만 사용
    if feature_names[i] == 'yr_renovated':
        plt.boxplot(data[data['yr_renovated']!=0]['yr_renovated'])
    else:
        plt.boxplot(data[feature_names[i]])
    plt.xlabel(feature_names[i])
    plt.show()
# 몇몇변수에서 아웃라이어 많아보임..

# 그냥 다중회귀 돌려보기
lm = linear_model.LinearRegression()
data.columns
feature_names=['bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot',
       'floors', 'waterfront', 'view', 'condition', 'grade', 'sqft_above',
       'sqft_basement', 'yr_built', 'yr_renovated', 'zipcode', 'lat', 'long',
       'sqft_living15', 'sqft_lot15', 'year', 'month', 'day', 'sqft_changed',
       'sqft_living_changed', 'sqft_lot_changed']
x = data[feature_names]
y = data['price']
lm.fit(x, y)
lm.coef_
lm.score(x,y)


# 상관관계 있어보이는 것만 돌려보기
feature_names=['bathrooms', 'sqft_living', 'sqft_lot',
       'condition', 'grade', 'sqft_above',
       'sqft_basement', 'yr_built', 'yr_renovated', 'lat', 'long',
       'sqft_living15', 'sqft_lot15', 'sqft_changed',
       'sqft_living_changed', 'sqft_lot_changed']
x = data[feature_names]
y = data['price']
lm.fit(x, y)
lm.coef_
lm.score(x,y)
# 스코어 떨어짐..

# 해야할 것
# 1. p-value 표시해주는 모듈 구해서 backward
# 2. 상관계수 구하기
# 3. 결측치 제거 (제거할 기준 설정, 논문찾아보기;)
# 4. 다른 분석모델적용
