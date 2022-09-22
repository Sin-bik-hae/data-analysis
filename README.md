# data-analysis
---
# 아이디어
1. 결제 계좌에 따른 소비
2. 개별적 소비성향 분석
연령별
스케일 조정
3. 범주 설정
4. 소비금액으로 등급을 나누어 분석
등급 설정 
- 신용평가 모형으로 등급 (머신러닝) 
+. 네이버 데이터랩
	- 카드 사용 통계

소비등급모형

서론 : 한정된 자원으로 모형을 만듬 … (개선 방안 : 어떤 자원이 주어지면  나아질 듯)

이런 점에서 의미가 있을 것 같다………→ 

- 새로운 변수를 만듦  1. 변수 묶음 2.
1. 데이터 
    1. 훈련 검증 8 : 2 
    2. 종속 변수 정의 : 총 지출액 or 예금률에 관련된 변수
    3. 종속변수의 비율 : 1:1이 아니면 오버샘플링 필요
2. 변수 처리 ??
    1. 극단치 처리 (하위 2%~ 상위 98%)
    2. (변수 변환) 
    3. 단변량  → T-Test, 단순 로지스틱 회귀분석, AR 및 K-S통계량을 활용하여 변수의 변별력을 검토
    4.  상관관계 분석 →단변량 분석을 통과한 변수들에 대해서 각 범주별로 상관분석을 진행 + 각 범주별 상관분석을 통과한 변수들에 대해서 전 체 변수 간의 상관관계를 검토하여 최종 후보 변수들 을선별
    
3. 각 방법론 별 파라미터 최적화 
    1. 로지스틱 : 높은 해석력, 안정적인 예측력
    2. 그래디언트 부스팅 : 높은 예측력 
    3. 다중 로지스틱 회귀모형에서 예측된 부도 확률값과 실측부도값과의 차이를 그래디언트 부스팅 을 통해 예측하는 모형을 도출
4. 평가 지표 
    1. 금융권 , 신용평가모형의 변별력 지표로 사용되는 k-s 통계량, AUROC 
    2. Accuracy, Recall, Precision, F1 Score

### 변수 설정

grouping 기준 지표 

1.%Event : (범주별 event 관측치 수 / 전체 event 관측칯 수 )* 100

2.odds : 범주별 non-event 관측치 수 / 범주별 event 관측치 수 

1. WOE : log(%non-event/%event)
2. IV : (%non-event - %event)*log(%non-event/%event)
