################### Matrix, List, Vector ############################

#### Matrix : Vector + 열,행 속성 지정한 배열데이터

## 1부터 20까지 4행 5열로 matrix 만들기(byrow=TRUE 일 경우, 열로 채우는게 아니라 row단위로 숫자를 채운다.)
(myMatrix <- matrix(1:20, nrow=4, ncol=5, byrow=TRUE))  
## byrow 속성 뺀 경우와 비교
(myMatrix <- matrix(1:20, nrow=4, ncol=5))  

myMatrix

## 매트릭스 길이보다 숫자범위가 짧은 경우, 숫자범위가 다시 처음부터 반복된다.
## 에러 메시지가 난다고 난게 아니다. 19까지 다 채운 후 다시 1부터 채운다
## 정해진 길이가 다 채워지면 추가가 안된다.
myMatrix <- matrix(1:19, nrow=4, ncol=5)

## 전체내용 출력
myMatrix

attributes(myMatrix)  ## 속성을 나타냄(해당 변수가 어떤 특징을 가지는지 나타낸다.)

myMatrix[-1,]  ## 1행은 제외하고 나머지를 출력하라!!

## Matrix 끝

#### List 
## 여러Type의 Vector를 묶어주는 형태
## [[2]] 이런 형태의 겹대괄호로 indexing 가능
c1NAme <- "이재현"
c1Age <- 46
c1hobby <- "등산"
c1Visit <- c("관악산", "북한산", "청계산")
customerDatabase <- list(name=c1NAme, age=c1Age, hobby=c1hobby, visit=c1Visit)
customerDatabase

customerDatabase[[2]]  ## indexing 형태 확인!!

## 만약 visit에서 북한산을 뽑고 싶다
customerDatabase[[4]][2]  ## [[4]]의 경우 visit Vector를 의미하며, 뒤의 [2] 의 경우 vector의 2번째 요소를 가리킨다 라고 생각

#### List 끝 

#### DataFrame 
## 길이가 같은 Vector를 모아서 만듦(NA : NULL 허용)
## data frame 생성 시 첫행은 변수 또는 속성이라 칭하며, 그 밑부분의 값들은 case 혹은 사례라 칭한다
## 즉 myFamilyNames,myFamilyAges ... : 변수 또는 속성 / Dad, Mon, sis ... : case 또는 사례
## 표의 첫행을 데이터에 의한 데이터 : 메타데이터 라고 할 수 있다.
## 시작열 및 행은 1번부터 시작한다.(이말은 indexing 할 경우 1번부터 시작) 
## dataframe의 특징은 한 열은 무조건 동일한 type의 값을 가진다.(Vector의 특성을 가지고 옴) 
## 모든 열의 수가 같다.(즉, 위에서 말한 길이가 같은 Vector의 집합을 의미)

myFamilyNames <- c("Dad", "Mom", "sis", "Bro", "Dog")
myFamilyAges <- c(43,42,12,8,5)
myFamilyGenders <- c("Male", "Female", "Female", "Male", "Female")
myFamilyWeights <- c(188, 136, 83, 61, 44)
myTest <- c(NA, NA, NA, NA, NA)  ## NULL 허용
myFamily <- data.frame(myFamilyNames, myFamilyAges, myFamilyGenders, myFamilyWeights, myTest)

myFamily
myFamily[1,2]  ## dataframe에서 변수찾기
myFamily[,2]  ##  2번째열의 모든 행값 뽑기

myFamily[-1, 2]  ## 첫째 행을 제외한 나머지 2열을 나타내!!

str(myFamily) ## dataframe 구조
str(myFamily, stringsAsFactors=TRUE);  ##문자

#### DataFrame  끝

#### Array 
myArray <- array(60:1, dim=c(4,5,3))  ## 60~1 까지 4행 5열의 3세트 배열을 만들어라
myArray  # 결과 확인

## 속성 확인
attributes(myArray)

myArray[1,2,1]  ## 고차원 배열의 접근
myArray[1,2,-1] 
myArray[1,2,]

#### Array 끝 

