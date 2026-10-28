# E05-115 HKS tracking stage diagnostics — v2

The original replay tree is not modified by these files. Apply patches only under `replay_revision`.

## Outputs

- `hrawRUN.root`: existing h9030 (event summary) and h9031 (one row per raw hit); unchanged.
- `hstageRUN.root`: new h9040 (one row per HKS event reaching `h_keep_results`) and h9041 (one row per retained Space Point candidate).
- `hsummaryRUN.root`: offline join of h9030 and h9040, keyed by (run,event); neither input is overwritten.

The original run number and event ID are stored as Float_t by HBOOK and h2root. Joining is exact only while integer IDs are exactly representable in Float_t (integer IDs up to 16,777,216). Do not pair files from different replay input ranges or parameter/software versions.

## 1. Stage instrumentation source patch

Transfer `e05115_stage_diagnostics_v2.patch` to JLab. From the revised source tree:

```bash
cd ~/E05-115/replay_revision/replay/e05115src
patch --dry-run -p1 < /path/to/e05115_stage_diagnostics_v2.patch
patch -p1 < /path/to/e05115_stage_diagnostics_v2.patch
```

The patch includes the new Fortran includes, h_stage_ntuple.f, and modified `h_clear_event`, `h_keep_results`, `g_ntuple_init`, `g_ntuple_shutdown`, ENGINE Makefile, `h_pattern_recognition`, `h_dc_tofcut`, `h_left_right`, and `h_link_stubs`. It does **not** replace your hraw diagnostic routines.

If you already applied the *older* stage diagnostic patch, don't apply this one on top without backing it out/reconciling; its new files and names overlap.

## 2. ROOT conversion from existing replay_script

```bash
cd ~/E05-115/replay_revision/replay/e05115replay
patch --dry-run -p1 < /path/to/e05115_hstage_replay_script.patch
patch -p1 < /path/to/e05115_hstage_replay_script.patch
```

This adds hstage conversion after the engine invocation, without removing existing hraw conversion.

## 3. Rebuild and run a small sample

```bash
./compile.sh > compile.log 2>&1
grep -nE 'Error:|No rule to make target|cannot stat' compile.log
ls -lh EXE/engine_replay
./replay_script 76312 gstart=1 gstop=10000
ls -lh rootfiles/hraw76312.root rootfiles/hstage76312.root
```

Do not interpret the presence of EXE/engine_replay alone as proof it was rebuilt; check compile.log and timestamps.

## 4. Initial checks (ROOT)

```cpp
TFile *a=TFile::Open("rootfiles/hraw76312.root");
TFile *b=TFile::Open("rootfiles/hstage76312.root");
TTree *raw=(TTree*)a->Get("h9030");
TTree *stage=(TTree*)b->Get("h9040");
TTree *sp=(TTree*)b->Get("h9041");
std::cout << raw->GetEntries() << " " << stage->GetEntries() << "\n";
stage->Print();
sp->Print();
stage->Scan("event:tofstat:pretotal:posttot:sp1:sp2:lrplan:lrfit:linkeval:linkpass","tofstat==2","",20);
stage->GetEntries("tofstat==2 && posttot>pretotal"); // expected 0
stage->GetEntries("lrfit>lrplan || linkpass>linkeval"); // expected 0
```

`tofstat`: 0 not called; -1 early exit in pattern recognition; 1 entered TOF routine but returned before applying cut; 2 applied cut/reordering. `spstat`,`lrstat`,`linkstat`: 0 not called, -1 skipped before candidate processing, 1 reached candidate processing. Counts of zero must not be confused with stages that were not executed.

`spraw1/2`: number returned by SP finding before per-chamber overflow discard and `h_choose_single_hit` / `select_space_points_hks`. `sp1/2`: retained SP candidates. `lrplan`: enumerated LR signs; `lrfit`: actual h_find_best_stub calls. `linkall`: SP1*SP2 only (not a count of all alternative/stub-fit pairs), `linkeval`: evaluated cross-chamber candidates after prefilters, `linkpass`: geometry matches; these are *not* all final tracks.

`h9041.nhits` is the hit count of each retained SP; `nalt` copies the existing `space_points_multi` value (zero means there is no multi-hit alternative). It is not independently enumerated hit combinations. `precut` is -1 if LR stage was never reached.

## 5. Offline event summary

Copy `make_hsummary.C` to your analysis working directory, then:

```bash
root -l -b -q 'make_hsummary.C("../replay/e05115replay/rootfiles/hraw76312.root","../replay/e05115replay/rootfiles/hstage76312.root","hsummary76312.root")'
root -l hsummary76312.root
```

ROOT commands:

```cpp
event_summary->Print();
event_summary->Draw("lrfit:rawtot","lrstat==1","COLZ");
event_summary->Draw("linkeval:posttot","tofstat==2 && linkstat==1","COLZ");
event_summary->Draw("sp1+sp2:rawtot","spstat==1","COLZ");
event_summary->Draw("lrfit:rawplane[2]","lrstat==1","COLZ"); // plane 3
```

The joining macro detects duplicate or missing `(run,event)` keys; verifies final-track counts and obvious impossible stage counts; and reports any disagreement between the raw diagnostic's decoded count and the stage diagnostic's pre-TOF count.

The C++ macro was syntax checked against ROOT-like header stubs; an actual ROOT runtime and HBOOK integration test must be performed on JLab.
