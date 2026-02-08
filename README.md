# MyTerminal-c (TabbyTerm Workspace)

Windows 전용 Rust/egui/wgpu 기반 터미널입니다.  
현재 버전은 블록 기반 출력, 탭/Pane, AI 패널, 커스텀 상단바, 애니메이션 마스코트를 포함합니다.

## 실행

```powershell
cargo run -p app
```

## 현재 UI 구성

1. 통합 상단바
- 메뉴(`File`, `Pane`, `View`, `Tools`)
- 창 버튼(최소화/최대화/닫기)
- 상단바 드래그로 윈도우 이동, 더블클릭 최대화/복원

2. 탭 바
- 탭 전환
- `+ new tab` 추가
- 각 탭 `x` 버튼으로 닫기

3. 좌측 사이드바
- 워크스페이스/트리/파일 검색/Git 요약

4. 중앙 패널
- 커맨드 블록 + AI 블록 타임라인
- Pane 레이아웃(single/vertical/horizontal)

5. 하단 입력바
- 명령 입력 및 실행
- 애니메이션 마스코트(이미지 프레임 로드 시 사용, 실패 시 ASCII fallback)

6. 상태바
- 상태 메시지, 블록 수, pane 상태, 탭, autosave 정보

## 단축키

1. `Ctrl+Shift+P`: Command Palette 열기
2. `Ctrl+B`: Sidebar 표시/숨김
3. `Ctrl+1`: Single Pane
4. `Ctrl+2`: Vertical Split
5. `Ctrl+3`: Horizontal Split
6. `Ctrl+W`: 활성 탭 닫기
7. `ArrowUp`/`ArrowDown`: 명령 히스토리 이동
8. `Enter`: 명령 실행
9. `Esc`: Command Palette 닫기

## 메뉴 기능

### File

1. New Terminal Tab
2. Close Active Tab
3. Export All Blocks / Export Bookmarks
4. Save Session Snapshot / Restore Session Snapshot
5. Recent Session Snapshots

### Pane

1. Single / Vertical / Horizontal 전환
2. Clear Active Pane

### View

1. Sidebar 토글
2. AI Panel 토글
3. Density(Compact / Comfortable / Spacious)

### Tools

1. Command Palette
2. Recent Commands
3. Clear Selected AI Context

## 블록 기반 워크플로우

1. 명령 실행 시 커맨드/출력이 Command Block으로 누적됩니다.
2. 블록별 북마크, 컨텍스트 선택, 복사, 삭제가 가능합니다.
3. 상단 검색으로 command/output/AI output 필터링이 가능합니다.
4. 출력에 `path:line[:column]` 패턴이 있으면 `open ...` 버튼으로 editor-open 명령을 입력창에 채웁니다.

## AI 패널

1. 도구: Codex CLI / Claude Code
2. 선택한 컨텍스트 블록을 자동 첨부하여 프롬프트 실행
3. AI 출력은 스트리밍 방식으로 타임라인에 계속 append됩니다.
4. 완료 시 `completed`, 실패 시 `failed` 상태로 기록됩니다.

## 애니메이션 마스코트

앱은 아래 경로에서 PNG 프레임을 자동 로드해 입력바 마스코트를 애니메이션으로 표시합니다.

1. `assets/mascot/gaming-cat`
2. `assets/mascot/party-parrot`

조건:

1. `.png` 파일만 사용
2. `._*` 같은 메타 파일은 자동 제외
3. 프레임을 못 읽으면 ASCII 런캣 프롬프트로 fallback

## 설정 파일

`config/config.toml`

주요 항목:

1. AI 실행 프로그램/인자
2. AI 타임아웃
3. 세션 autosave 주기
4. 세션 파일 경로

앱 실행 중 파일 변경 시 주기적으로 hot reload 됩니다.

## 세션 저장/복원

1. autosave: 설정 주기마다 저장
2. 수동 저장: 메뉴 `Save Session Snapshot`
3. 복원: 메뉴 `Restore Session Snapshot`
4. 종료 시 현재 워크스페이스 스냅샷 저장

## 개발 검증

```powershell
cargo fmt
cargo check
cargo test
```

## 현재 제한사항

1. Pane는 레이아웃/포커스 중심이며 pane별 독립 PTY 분리는 아직 미구현입니다.
2. 파일/라인 링크는 명령 자동 채움 방식이며 자동 실행은 하지 않습니다.
3. 마스코트는 현재 PNG 시퀀스 자동 로드 방식입니다.
