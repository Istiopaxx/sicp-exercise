# SICP EXERCISE
SICP EXERCISE 모음집

- [원본 리드미](https://github.com/murry2018/wizardbook)

## 진행방식

- 지정된 분량을 읽고 문제를 풀어옵니다.
- 각자 깃허브 리포지토리에 풀이를 올립니다.
- 스터디 당일 문제-참여자 랜덤 매칭하여 해설합니다.

## Scheme 설치

- Sicp에선 프로그래밍 언어 LISP의 방언인 Scheme을 사용합니다. 그런데 Sicp에서 사용한 Scheme 버전이 오래되어 해당 환경의 실행기를 구하기 힘듭니다.
- 따라서 LISP의 방언인 [Racket(설치링크)](https://download.racket-lang.org/)에서 Sicp버전의 Scheme 구현체를 이용하여 문제를 풀어봅니다. 
- Racket을 설치하면, racket 실행기와 DrRacket 실행기, 그리고 racket용 패키지 매니저인 raco가 설치됩니다.
- Racket이 설치된 폴더에 raco 실행파일도 같이 있습니다. PATH에 추가하고, 터미널에서 `raco pkg install racket-langserver`를 실행하여 패키지를 다운받습니다.
- VSCODE를 키고, 익스텐션에서 [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket) 검색 후 install
- 소스코드(.rkt) 최상단에 `#lang sicp`를 작성해주면 실행기가 자동으로 Sicp 버전으로 문법을 해석합니다. 

### 스터디 교재
**SICP(Structure and Interpretation of Computer Program) 2/E**

- 번역서(정가 43,000원)

  ![SICP](https://github.com/murry2018/wizardbook/blob/master/sicp.jpg?raw=true)
  
  *컴퓨터 프로그램의 구조와 해석, 김재우 외 옮김*
- 원서([html 버전](https://mitpress.mit.edu/sites/default/files/sicp/index.html))

  ![SICP Banner](https://github.com/murry2018/wizardbook/blob/master/sicp-banner.gif?raw=true)
  
  *원서의 html 버전은 공개 되어있음.*

### 스터디 참고 자료
- [Racket 설치 및 SICP 모듈 설정](https://kkalkkalparrot.tistory.com/32)
- [옮긴이의 글(긴 버전)](http://pchero21.com/?p=361)
- [커뮤니티 솔루션](http://community.schemewiki.org/?SICP-Solutions) : 해답지는 원래 없습니다.

### 스터디 목표
이 목표는 단지 그 주에 함께  살펴볼 진도일 뿐입니다. 개인의 역량이나 시간에 따라 더 진도를 나가도 좋습니다!
- [x] 9월 2주차: 서문 + 1.1절(2021-09-12 16시)
- [ ] 9월 3주차: 1.2절

## 스터디 참가자 일람
- [@murry2018](https://github.com/murry2018/wizardbook)
- [@nicksuh](https://github.com/nicksuh/SICP_exercises/)
- [@KeisLuv5991](https://github.com/KeisLuv5991/SicpExercise)
- [@fienestar](https://github.com/fienestar/sicp)
- [@dsyun96](https://github.com/dsyun96/wizard-practice)
- [@blurfx](https://github.com/blurfx/sicp)
- [@rosqxkedrt](https://github.com/rosqxkedrt/sicp_study)
