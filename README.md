# 주의사항

- 개인 작업 내용은 'personal' branch 에 올려주세요.
- main branch 는 협업이 필요하거나 특정 버전의 결과물을 배포할 때 사용하려고 합니다.

---

# Git 사용법

```bash
# 먼저 자신의 git 폴더로 이동하여 우클릭 -> git bash를 선택하여 bash 창을 띄웁니다.
$ git status # check your git folder
```

```bash
# 다른 사람의 수정 사항을 현재 내 컴퓨터에 반영할 때, 내 컴퓨터의 파일을 최신 버전으로 업데이트
$ git pull

# 내 수정사항을 서버로 보내는 방법
$ git add .
$ git commit -m "<메시지>"
$ git push origin <branch, 우리는 personal을 주로 사용할 것>
```

아마 처음 사용할 때 `push` 명렁어 사용 시 에러가 뜰 수 있는데, 이 때는 아래 코드를 실행해주세요.

```bash
$ git config --global user.name "Your Name"        # github nickname
$ git config --global user.email you@example.com   # github email
```

---

# 아이디어

1. (경윤) 소비 등급을 나누어 씀씀이에 따른 소비 패턴을 분석
   
   - 사용자의 소비 등급에 맞는 혜택을 줄 수 있음.

2. (찬우) 2%의 분야에서 신한카드를 사용하지만 신한은행 고객이 아님. 의료분야. 어떤 이유로 그런 양상이 나타나는지.

3. (태순) 가장 큰 그룹을 분류하는 기준: 은행이랑 1,0
   
   - 은행: B 은행의 고객을 A 은행으로 끌어들이고자
   
   - 1,0: P3가 0인 고객을 1로 끌어들이고자

4. 패턴코드로 시즌 아이템을 생각해보는 것도

5. 
