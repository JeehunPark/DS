library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(stringr)
library(stringi)
library(rJava)
library(tm)

# 자바 경로 설정
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre-9.0.1")

# gc
gc(verbose = T)

# 초록입력
abstract = c("현재 건설시장에서는 시공의 기계화 및 로봇화 촉진을 통하여 인력의존형으로부터 탈피하고자 하는 노력이 지속적으로 이어져 오고 있다. 본 연구에서는 건설생산의 효율성 제고를 위한 건설기계장비의 활용도 향상을 위해 국내 외 건설기계 자동화와 관련된 연구개발 동향을 살펴보고, R&D 투자현황을 조사 분석하였다. 또한 건축과 토목공사에 있어 건설자동화 분류체계를 수립하고, 자동화 대상작업의 후보군을 선정한 후 설문조사를 통하여 자동화 대상작업의 우선순위를 도출하였다. 도출된 자동화 우선순위를 바탕으로 총괄로드맵을 작성하였으며, 자동화 단위 그룹과 단위 그룹 내 자동화 대상작업의 자동화에 필수적으로 요구되는 요소기술의 도출결과를 바탕으로 중점영역별 로드맵을 구축하여 제시하였다. 본 연구의 결과물은 국내 건설 생산방식의 성력화 및 기술집약화를 위한 첨단 기술분야 R&D 전략수립의 기초자료로 활용될 수 있을 것으로 기대된다.", 
             "고속도로는 국가 기간 교통망으로 1970년 경부고속도로가 개통된 이래로 날로 급증하는 교통수요와 다양한 고객의 요구에 부응하기 위해 지속적으로 건설되었다. 지속적인 고속도로 건설사업의 추진에 따라 건설 기술인들의 많은 관심과 노력으로 교량, 터널, 기타 구조물 시공 등 건설 기술적 부분은 세계적 수준에 이르렀다. 하지만, 아쉽게도 고속도로 건설단계의 안전관리에 대한 관심과 관리는 건설 기술 수준에 비해 상대적으로 미흡한 실정이다. 고속도로 건설공사의 재해는 매년 감소하고 있는 추세이지만 고속도로 건설공사 시 발생되는 재해 특성에 맞는 적절한 안전관리가 이루어지지 않으면 안전사고는 중대재해 형태로 이어져 그에 따른 직·간접적인 손실과 피해는 매우 크게 나타난다. 따라서 고속도로 건설재해 발생현황을 분석하여 건설 기술 수준에 맞는 안전관리 방안 모색이 필요하며, 그에 따른 체계적이고 합리적인 안전관리가 이루어져야 한다. 본 연구는 일반 건설재해 발생현황을 중심으로 현황분석을 실시하고, 건설재해 중 고속도로 재해를 재분류하여 그 특성에 따른 현황분석을 실시하였다. 현황분석 결과 도출된 주요 재해 취약요인들을 중심으로 고속도로 건설재해를 예방하기 위한 중점 관리 항목으로 선정하고, 중점 관리 항목들에 대한 안전관리 개선방안을 제시하고자 한다.", 
             "국내 건설 산업은 지속적 성장에도 불구하고 선진국 대비 건설 기술 경쟁력은 여전히 부족하다. 또한 매년 원자재 가격은 상승을 하고 있으며 타 산업에 비해 낮은 연구 개발비 투자는 저조한 수준이다. 이에 건설 산업은 국가 발전과 세계 경쟁력 제고를 위해 정부는 건설 신기술 개발 촉진을 위해 1989년 건설 신기술 지정제도를 도입하여 운영하고 있다. 하지만 신기술의 현장 도입 시 현장 적용 효과의 불확실성, 기존 공법에 대한 기술적 우위의 확신 결여 등으로 인하여 건설 신기술에 대한 현장 적용 활용 빈도가 현저히 낮은 것으로 밝혀졌다. 이러한 건설 신기술의 현장 적용 활성화를 위해 신기술 적용시 성능평가를 위한 예측 자료 및 신뢰성 높은 예측 방법의 개발이 필요한 실정이다. 본 연구에서는 앞서 제기된 건설 신기술의 현장 적용 활성화를 위해 건설 신기술로 지정된 10개 분야 569건의 건설 신기술을 검토하였다. 공정 액티비티 구별이 가능하고 기존 공법과의 공정에 있어서의 차이가 확연한 동시에 객관적인 생산성 및 비용 자료 수집이 가능한 기술을 연구대상으로 지정하였다. 첫 단계로서 위에서 언급한 신기술 대상 선정 기준에 부합된 철골 시스템 계단 공법을 사례 연구 대상으로 선정하였으며, 비교대상인 기존 공법으로 철근콘크리트 계단 공법으로 선정하였다. 선정된 두 공법이 적용중인 현장 방문을 통하여 작업인원, 기계장비, 작업시간, 원가자료 등 현장 실측 데이터를 수집하였다. 이러한 데이터를 기반으로 건설 시뮬레이션 기법을 적용하여 실측 데이터와 건설 시뮬레이션 데이터의 검증을 실시 후 cycle에 따른 예상 작업시간 데이터를 도출하였다. 또한 시스템 철골 계단 공법과 철근콘크리트 계단 공법의 노무비, 자재비, 장비비에 따른 단위비용 비교 분석을 실시했다. 기존 철근콘크리트 계단 공사시 골조 공사와 함께 동시 진행 공사보다 시스템 철골 계단 공법 적용시 반복작업으로 인한 공사가 더 효율적인 데이터를 얻었다. 본 연구는 건설신기술의 현장 적용성을 효과적으로 평가하기 위하여 건설시뮬레이션 기법을 사용하였으며, 연구 결과 기존 공법 대비 생산성 및 비용에 대한 비교 분석을 수치화를 통해 객관화 할 수 있음을 제시하였다. 이에 건설 시뮬레이션 기법을 건설 신기술의 현장 적용성을 평가함에 있어 적절한 기법 중 하나임이 제시되고 있으며, 이는 향후 구체적인 현장 데이터를 바탕으로 건설 신기술의 성능을 평가하고 예측할 수 있는 사용이 보다 용이하며 신뢰성 높은 방법론의 개발을 위한 기초연구로서 실행되었다.", 
             "최근 들어 정보기술의 발전과 더불어 수많은 다양하고 형태의 데이터들이 기하급수적으로 방대하게 늘어나고 있어 많은 복잡한 데이터의 분석에 대한 필요성이 여러 분야에서 대두 되고 있다. 다량의 복잡한 데이터를 분석하고 원하는 결과로 나타내기위한 방법으로 머신러닝이 각광받고 있다. 머신러닝이란 어떠한 명시적인 프로그램 없이도 스스로 학습하는 능력을 갖춘 컴퓨터로 제공되는 데이터 인공지능 분야의 한 종류이며 새로운 데이터가 입력되면 스스로 학습하여 자신을 향상시키고 변화시키는 컴퓨터 프로그램을 바탕으로 동작한다. 머신러닝은 크게 지도학습과 비지도학습으로 분류할 수 있고, 지도학습은 분류와 회귀 등의 알고리즘들이 있고, 비지도학습은 군집화와 밀도추정 등의 알고리즘들이 있다. 본 논문에서는 분류기중에서도 많이 사용되어 비교되어지는 k-최근접 이웃(k-Nearest Neighbor: k-NN)과 서포트벡터머신(Support Vector Machine: SVM) 분류기(Classifier) 알고리즘 두 개를 실험하여 비교 평가하였다. 두 알고리즘을 실험하기 위해서 표본 데이터로 어린이집에서 보유한 영유아 건강검진 신체 데이터 중에 나이, 성별, 신장, 몸무게 정보만을 따로 분류 수집하여 이용하였다. 그중에 분류를 위한 수치정보는 신장과 몸무게를 이용하였고 데이터집합의 항목 값으로는 나이를 사용하였다. k-NN은 비모수적 밀도추정에 기반을 두고 가장 가까운 k개의 인접 데이터를 취하여 다수결을 통해 해당 데이터집합의 항목 값을 분류할 데이터의 항목 값으로 결정하는 방식이다. SVM은 기본적으로 이진선형분류에 사용되어지며 분류를 원하는 집합 사이의 가장 큰 폭의 경계에 위치한 서포트벡터 데이터를 기준으로 선형경계를 이용하여 두 집단을 분리시키는 알고리즘이다. SVM은 선형분리뿐 아니라 비선형분리에도 사용 할 수 있지만 주어진 데이터를 고차원으로 사상시키는 커널(Kernel) 방법을 사용해야한다. 원천수집데이터는 100개뿐이라서 본 실험에서 사용되어지는 500개, 1,000개 그리고 10,000개의 실험을 하기위해서는 데이터를 늘릴 필요가 있었다. 데이터개수를 늘리는 방법으로는 모든 각각의 데이터 임의의 반경 내에서 100의 배수만큼 무작위로 생성시키는 방법을 취하였다. k-NN의 알고리즘은 분류할 데이터가 입력 될 때마다 다시 모든 데이터를 학습하는 방식이고 SVM은 미리 학습하여 미리 분리할 경계를 찾고 분류할 대상인 데이터가 입력되면 학습만 별도로 수행이 가능한 알고리즘 방식이다. 비교실험은 두 분류기의 분류하는 소요시간과 분류 정확도를 실험데이터 크기별로 비교 실험 하였다. 본 논문을 통하여 두 분류기의 이론적인 내용을 심화 학습하고 분류기라는 공통적인 머신러닝 알고리즘을 각각 실험하여 비교 평가하였다. 비교 평가한 결과물은 다른 종류의 데이터를 분류하고자 할 때 적당한 분류기를 선택 할 수 있는 기준을 제시한다.")

# 사용자 사전 단어 등록
buildDictionary(ext_dic = "NIADic", 
                user_dic = data.frame(term = c("서포트백터머신", "k-NN", "머신러닝"), tag = c("f", "f", "ncn")))

# 초록 전처리(특수문자제거)
content = str_replace_all(abstract[4], "\\)|\\(|\\.|:", " ")

# 명사추출
ext = extractNoun(content, autoSpacing = F)

# 한 글자 단어 제거
a = ext[nchar(ext)>1]

# 빈도추출 및 내림차순정렬
b = sort(table(a), decreasing = T)


windowsFonts()
font = windowsFonts(malgun=windowsFont("맑은 고딕"))
font = windowsFonts(chun=windowsFont("경기천년제목 Light"))


wordcloud(names(b), b, min.freq = 1, colors = brewer.pal(9, "Set1"), random.color = T, random.order = F, family = "chun")

wordcloud2(data = b, shape = "star", size = 1, minSize = 0, fontFamily = font$chun, backgroundColor = "white")
wordcloud2(data = b, shape = "triangle", size = 1, minSize = 0, fontFamily = font$chun)
wordcloud2(data = b, shape = "circle", size = 1, minSize = 0, fontFamily = font$chun)
wordcloud2(data = b, shape = "circle", size = 1, minSize = 0, fontFamily = font$malgun)


# http://freesearch.pe.kr/archives/3138
