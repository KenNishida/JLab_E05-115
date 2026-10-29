// ENGINE_revision htracking: complete nine-tree, per-event provenance buffer.
#include <TFile.h>
#include <TTree.h>
#include <TObject.h>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <limits>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace {
constexpr double NaN = std::numeric_limits<double>::quiet_NaN();
struct Base { int32_t run = -1, event = -1; };
struct EventsRow : Base {
  int32_t event_type = -1;
  int32_t n_raw[12] = {};
  int32_t n_decoded[12] = {};
  int32_t n_sp[2] = {};
  int32_t n_hitsets = -1;
  int32_t n_lr_generated = -1;
  int32_t n_lr_fitted = -1;
  int32_t n_links = -1;
  int32_t n_tracks_generated = -1;
  int32_t n_tracks_fitted = -1;
  int32_t n_tracks_final = -1;
  int32_t tof_cut_status = -1;
};
struct RawHitsRow : Base {
  int32_t raw_id = -1;
  int32_t layer = -1;
  int32_t wire = -1;
  int32_t tdc = -1;
  int32_t dec_id = -1;
  int32_t decode_status = -1;
};
struct DecodedHitsRow : Base {
  int32_t dec_id = -1;
  int32_t raw_id = -1;
  int32_t layer = -1;
  int32_t wire = -1;
  double wire_center = NaN;
  double drift_time_initial = NaN;
  double drift_time_pre = NaN;
  double drift_time = NaN;
  double drift_distance = NaN;
  int32_t tof_pass = -1;
};
struct SpacePointsRow : Base {
  int32_t sp_id = -1;
  int32_t chamber = -1;
  double x_sp = NaN;
  double y_sp = NaN;
  int32_t n_hits = -1;
  int32_t n_hitsets = -1;
  int32_t n_lr_generated = -1;
  int32_t n_lr_fitted = -1;
  int32_t best_hitset_id = -1;
  int32_t best_lr_id = -1;
  double stub_x = NaN;
  double stub_y = NaN;
  double stub_xp = NaN;
  double stub_yp = NaN;
  double stub_chi2 = NaN;
  int32_t sp_selected = -1;
  int32_t precut_pass = -1;
};
struct HitSetsRow : Base {
  int32_t hitset_id = -1;
  int32_t sp_id = -1;
  int32_t n_hits = -1;
  std::vector<int32_t> hit_ids;
  int32_t n_lr_generated = -1;
  int32_t n_lr_fitted = -1;
  int32_t best_lr_id = -1;
  double best_lr_chi2 = NaN;
  int32_t is_selected = -1;
};
struct LRHypothesesRow : Base {
  int32_t lr_id = -1;
  int32_t sp_id = -1;
  int32_t hitset_id = -1;
  int64_t lr_mask = -1; // up to 50 drift-chamber hits
  int32_t n_hits = -1;
  double chi2 = NaN;
  int32_t ndf = -1;
  double fit_x = NaN;
  double fit_y = NaN;
  double fit_xp = NaN;
  double fit_yp = NaN;
  int32_t is_best_in_hitset = -1;
  int32_t is_best_in_sp = -1;
};
struct StubLinksRow : Base {
  int32_t link_id = -1;
  int32_t sp1_id = -1;
  int32_t sp2_id = -1;
  int32_t lr1_id = -1;
  int32_t lr2_id = -1;
  double dx = NaN;
  double dy = NaN;
  double dxp = NaN;
  double dyp = NaN;
  int32_t link_pass = -1;
};
struct TracksRow : Base {
  int32_t track_id = -1;
  std::vector<int32_t> sp_ids;
  std::vector<int32_t> link_ids;
  int32_t n_sp = -1;
  int32_t n_hits = -1;
  double x_fp = NaN;
  double y_fp = NaN;
  double xp_fp = NaN;
  double yp_fp = NaN;
  double chi2 = NaN;
  int32_t ndf = -1;
  double chi2_ndf = NaN;
  int32_t final_track_id = -1;
};
struct TrackHitsRow : Base {
  int32_t track_id = -1;
  int32_t dec_id = -1;
  int32_t sp_id = -1;
  int32_t layer = -1;
  int32_t wire = -1;
  int32_t lr_sign = -1;
  double wire_coord = NaN;
  double track_coord = NaN;
  double residual = NaN;
  double drift_distance = NaN;
  double sigma = NaN;
};
class Writer {
public:
  std::unique_ptr<TFile> file;
  bool active = false;
  int32_t run = -1, event = -1, eventtype = -1;
  EventsRow ev;
  std::vector<RawHitsRow> raws;
  std::vector<DecodedHitsRow> decs;
  std::unordered_map<int32_t,size_t> rawpos, decpos;
  std::vector<SpacePointsRow> spaces;
  std::vector<HitSetsRow> hitsets;
  std::unordered_map<int32_t,size_t> sppos, hitsetpos;
  std::unordered_map<int32_t,std::vector<size_t>> spHitsets;
  std::vector<LRHypothesesRow> lrs;
  std::vector<StubLinksRow> links;
  std::vector<TracksRow> tracks;
  std::vector<TrackHitsRow> trackhits;
  std::unordered_map<int32_t,size_t> trackpos;
  std::unordered_map<int32_t,std::vector<size_t>> trackhitpos;
  int32_t next_lr_id = 0;
  int32_t n_track_fit_attempts = 0;
  int32_t n_lr_fit_attempts = 0;

  TTree *tEvents = nullptr;
  EventsRow bEvents;
  TTree *tRawHits = nullptr;
  RawHitsRow bRawHits;
  TTree *tDecodedHits = nullptr;
  DecodedHitsRow bDecodedHits;
  TTree *tSpacePoints = nullptr;
  SpacePointsRow bSpacePoints;
  TTree *tHitSets = nullptr;
  HitSetsRow bHitSets;
  TTree *tLRHypotheses = nullptr;
  LRHypothesesRow bLRHypotheses;
  TTree *tStubLinks = nullptr;
  StubLinksRow bStubLinks;
  TTree *tTracks = nullptr;
  TracksRow bTracks;
  TTree *tTrackHits = nullptr;
  TrackHitsRow bTrackHits;
  void book() {
    tEvents = new TTree("Events", "HKS KDC Events");
    tEvents->Branch("run", &bEvents.run, "run/I");
    tEvents->Branch("event", &bEvents.event, "event/I");
    tEvents->Branch("event_type", &bEvents.event_type, "event_type/I");
    tEvents->Branch("n_raw", bEvents.n_raw, "n_raw[12]/I");
    tEvents->Branch("n_decoded", bEvents.n_decoded, "n_decoded[12]/I");
    tEvents->Branch("n_sp", bEvents.n_sp, "n_sp[2]/I");
    tEvents->Branch("n_hitsets", &bEvents.n_hitsets, "n_hitsets/I");
    tEvents->Branch("n_lr_generated", &bEvents.n_lr_generated, "n_lr_generated/I");
    tEvents->Branch("n_lr_fitted", &bEvents.n_lr_fitted, "n_lr_fitted/I");
    tEvents->Branch("n_links", &bEvents.n_links, "n_links/I");
    tEvents->Branch("n_tracks_generated", &bEvents.n_tracks_generated, "n_tracks_generated/I");
    tEvents->Branch("n_tracks_fitted", &bEvents.n_tracks_fitted, "n_tracks_fitted/I");
    tEvents->Branch("n_tracks_final", &bEvents.n_tracks_final, "n_tracks_final/I");
    tEvents->Branch("tof_cut_status", &bEvents.tof_cut_status, "tof_cut_status/I");
    tRawHits = new TTree("RawHits", "HKS KDC RawHits");
    tRawHits->Branch("run", &bRawHits.run, "run/I");
    tRawHits->Branch("event", &bRawHits.event, "event/I");
    tRawHits->Branch("raw_id", &bRawHits.raw_id, "raw_id/I");
    tRawHits->Branch("layer", &bRawHits.layer, "layer/I");
    tRawHits->Branch("wire", &bRawHits.wire, "wire/I");
    tRawHits->Branch("tdc", &bRawHits.tdc, "tdc/I");
    tRawHits->Branch("dec_id", &bRawHits.dec_id, "dec_id/I");
    tRawHits->Branch("decode_status", &bRawHits.decode_status, "decode_status/I");
    tDecodedHits = new TTree("DecodedHits", "HKS KDC DecodedHits");
    tDecodedHits->Branch("run", &bDecodedHits.run, "run/I");
    tDecodedHits->Branch("event", &bDecodedHits.event, "event/I");
    tDecodedHits->Branch("dec_id", &bDecodedHits.dec_id, "dec_id/I");
    tDecodedHits->Branch("raw_id", &bDecodedHits.raw_id, "raw_id/I");
    tDecodedHits->Branch("layer", &bDecodedHits.layer, "layer/I");
    tDecodedHits->Branch("wire", &bDecodedHits.wire, "wire/I");
    tDecodedHits->Branch("wire_center", &bDecodedHits.wire_center, "wire_center/D");
    tDecodedHits->Branch("drift_time_initial", &bDecodedHits.drift_time_initial, "drift_time_initial/D");
    tDecodedHits->Branch("drift_time_pre", &bDecodedHits.drift_time_pre, "drift_time_pre/D");
    tDecodedHits->Branch("drift_time", &bDecodedHits.drift_time, "drift_time/D");
    tDecodedHits->Branch("drift_distance", &bDecodedHits.drift_distance, "drift_distance/D");
    tDecodedHits->Branch("tof_pass", &bDecodedHits.tof_pass, "tof_pass/I");
    tSpacePoints = new TTree("SpacePoints", "HKS KDC SpacePoints");
    tSpacePoints->Branch("run", &bSpacePoints.run, "run/I");
    tSpacePoints->Branch("event", &bSpacePoints.event, "event/I");
    tSpacePoints->Branch("sp_id", &bSpacePoints.sp_id, "sp_id/I");
    tSpacePoints->Branch("chamber", &bSpacePoints.chamber, "chamber/I");
    tSpacePoints->Branch("x_sp", &bSpacePoints.x_sp, "x_sp/D");
    tSpacePoints->Branch("y_sp", &bSpacePoints.y_sp, "y_sp/D");
    tSpacePoints->Branch("n_hits", &bSpacePoints.n_hits, "n_hits/I");
    tSpacePoints->Branch("n_hitsets", &bSpacePoints.n_hitsets, "n_hitsets/I");
    tSpacePoints->Branch("n_lr_generated", &bSpacePoints.n_lr_generated, "n_lr_generated/I");
    tSpacePoints->Branch("n_lr_fitted", &bSpacePoints.n_lr_fitted, "n_lr_fitted/I");
    tSpacePoints->Branch("best_hitset_id", &bSpacePoints.best_hitset_id, "best_hitset_id/I");
    tSpacePoints->Branch("best_lr_id", &bSpacePoints.best_lr_id, "best_lr_id/I");
    tSpacePoints->Branch("stub_x", &bSpacePoints.stub_x, "stub_x/D");
    tSpacePoints->Branch("stub_y", &bSpacePoints.stub_y, "stub_y/D");
    tSpacePoints->Branch("stub_xp", &bSpacePoints.stub_xp, "stub_xp/D");
    tSpacePoints->Branch("stub_yp", &bSpacePoints.stub_yp, "stub_yp/D");
    tSpacePoints->Branch("stub_chi2", &bSpacePoints.stub_chi2, "stub_chi2/D");
    tSpacePoints->Branch("sp_selected", &bSpacePoints.sp_selected, "sp_selected/I");
    tSpacePoints->Branch("precut_pass", &bSpacePoints.precut_pass, "precut_pass/I");
    tHitSets = new TTree("HitSets", "HKS KDC HitSets");
    tHitSets->Branch("run", &bHitSets.run, "run/I");
    tHitSets->Branch("event", &bHitSets.event, "event/I");
    tHitSets->Branch("hitset_id", &bHitSets.hitset_id, "hitset_id/I");
    tHitSets->Branch("sp_id", &bHitSets.sp_id, "sp_id/I");
    tHitSets->Branch("n_hits", &bHitSets.n_hits, "n_hits/I");
    tHitSets->Branch("hit_ids", &bHitSets.hit_ids);
    tHitSets->Branch("n_lr_generated", &bHitSets.n_lr_generated, "n_lr_generated/I");
    tHitSets->Branch("n_lr_fitted", &bHitSets.n_lr_fitted, "n_lr_fitted/I");
    tHitSets->Branch("best_lr_id", &bHitSets.best_lr_id, "best_lr_id/I");
    tHitSets->Branch("best_lr_chi2", &bHitSets.best_lr_chi2, "best_lr_chi2/D");
    tHitSets->Branch("is_selected", &bHitSets.is_selected, "is_selected/I");
    tLRHypotheses = new TTree("LRHypotheses", "HKS KDC LRHypotheses");
    tLRHypotheses->Branch("run", &bLRHypotheses.run, "run/I");
    tLRHypotheses->Branch("event", &bLRHypotheses.event, "event/I");
    tLRHypotheses->Branch("lr_id", &bLRHypotheses.lr_id, "lr_id/I");
    tLRHypotheses->Branch("sp_id", &bLRHypotheses.sp_id, "sp_id/I");
    tLRHypotheses->Branch("hitset_id", &bLRHypotheses.hitset_id, "hitset_id/I");
    tLRHypotheses->Branch("lr_mask", &bLRHypotheses.lr_mask, "lr_mask/L");
    tLRHypotheses->Branch("n_hits", &bLRHypotheses.n_hits, "n_hits/I");
    tLRHypotheses->Branch("chi2", &bLRHypotheses.chi2, "chi2/D");
    tLRHypotheses->Branch("ndf", &bLRHypotheses.ndf, "ndf/I");
    tLRHypotheses->Branch("fit_x", &bLRHypotheses.fit_x, "fit_x/D");
    tLRHypotheses->Branch("fit_y", &bLRHypotheses.fit_y, "fit_y/D");
    tLRHypotheses->Branch("fit_xp", &bLRHypotheses.fit_xp, "fit_xp/D");
    tLRHypotheses->Branch("fit_yp", &bLRHypotheses.fit_yp, "fit_yp/D");
    tLRHypotheses->Branch("is_best_in_hitset", &bLRHypotheses.is_best_in_hitset, "is_best_in_hitset/I");
    tLRHypotheses->Branch("is_best_in_sp", &bLRHypotheses.is_best_in_sp, "is_best_in_sp/I");
    tStubLinks = new TTree("StubLinks", "HKS KDC StubLinks");
    tStubLinks->Branch("run", &bStubLinks.run, "run/I");
    tStubLinks->Branch("event", &bStubLinks.event, "event/I");
    tStubLinks->Branch("link_id", &bStubLinks.link_id, "link_id/I");
    tStubLinks->Branch("sp1_id", &bStubLinks.sp1_id, "sp1_id/I");
    tStubLinks->Branch("sp2_id", &bStubLinks.sp2_id, "sp2_id/I");
    tStubLinks->Branch("lr1_id", &bStubLinks.lr1_id, "lr1_id/I");
    tStubLinks->Branch("lr2_id", &bStubLinks.lr2_id, "lr2_id/I");
    tStubLinks->Branch("dx", &bStubLinks.dx, "dx/D");
    tStubLinks->Branch("dy", &bStubLinks.dy, "dy/D");
    tStubLinks->Branch("dxp", &bStubLinks.dxp, "dxp/D");
    tStubLinks->Branch("dyp", &bStubLinks.dyp, "dyp/D");
    tStubLinks->Branch("link_pass", &bStubLinks.link_pass, "link_pass/I");
    tTracks = new TTree("Tracks", "HKS KDC Tracks");
    tTracks->Branch("run", &bTracks.run, "run/I");
    tTracks->Branch("event", &bTracks.event, "event/I");
    tTracks->Branch("track_id", &bTracks.track_id, "track_id/I");
    tTracks->Branch("sp_ids", &bTracks.sp_ids);
    tTracks->Branch("link_ids", &bTracks.link_ids);
    tTracks->Branch("n_sp", &bTracks.n_sp, "n_sp/I");
    tTracks->Branch("n_hits", &bTracks.n_hits, "n_hits/I");
    tTracks->Branch("x_fp", &bTracks.x_fp, "x_fp/D");
    tTracks->Branch("y_fp", &bTracks.y_fp, "y_fp/D");
    tTracks->Branch("xp_fp", &bTracks.xp_fp, "xp_fp/D");
    tTracks->Branch("yp_fp", &bTracks.yp_fp, "yp_fp/D");
    tTracks->Branch("chi2", &bTracks.chi2, "chi2/D");
    tTracks->Branch("ndf", &bTracks.ndf, "ndf/I");
    tTracks->Branch("chi2_ndf", &bTracks.chi2_ndf, "chi2_ndf/D");
    tTracks->Branch("final_track_id", &bTracks.final_track_id, "final_track_id/I");
    tTrackHits = new TTree("TrackHits", "HKS KDC TrackHits");
    tTrackHits->Branch("run", &bTrackHits.run, "run/I");
    tTrackHits->Branch("event", &bTrackHits.event, "event/I");
    tTrackHits->Branch("track_id", &bTrackHits.track_id, "track_id/I");
    tTrackHits->Branch("dec_id", &bTrackHits.dec_id, "dec_id/I");
    tTrackHits->Branch("sp_id", &bTrackHits.sp_id, "sp_id/I");
    tTrackHits->Branch("layer", &bTrackHits.layer, "layer/I");
    tTrackHits->Branch("wire", &bTrackHits.wire, "wire/I");
    tTrackHits->Branch("lr_sign", &bTrackHits.lr_sign, "lr_sign/I");
    tTrackHits->Branch("wire_coord", &bTrackHits.wire_coord, "wire_coord/D");
    tTrackHits->Branch("track_coord", &bTrackHits.track_coord, "track_coord/D");
    tTrackHits->Branch("residual", &bTrackHits.residual, "residual/D");
    tTrackHits->Branch("drift_distance", &bTrackHits.drift_distance, "drift_distance/D");
    tTrackHits->Branch("sigma", &bTrackHits.sigma, "sigma/D");
  }
  void clear() {
    raws.clear(); decs.clear(); rawpos.clear(); decpos.clear();
    spaces.clear(); hitsets.clear(); sppos.clear(); hitsetpos.clear();
    spHitsets.clear(); lrs.clear(); links.clear(); tracks.clear();
    trackhits.clear(); trackpos.clear(); trackhitpos.clear();
    next_lr_id=0; n_track_fit_attempts=0; n_lr_fit_attempts=0;
    run = event = eventtype = -1;
    ev = EventsRow{};
  }
  void begin(int r, int e, int type) {
    // h_clear_event already cleared; also tolerate a missing clear.
    clear(); run = r; event = e; eventtype = type;
    ev.run = r; ev.event = e; ev.event_type = type;
  }
  void raw(int id, int layer, int wire, int tdc) {
    if (!active || event < 0) return;
    if (rawpos.count(id)) return;
    RawHitsRow h; h.run=run; h.event=event; h.raw_id=id;
    h.layer=layer; h.wire=wire; h.tdc=tdc;
    h.dec_id=-1; h.decode_status=-1;
    rawpos[id]=raws.size(); raws.push_back(h);
  }
  void decodeStarted() {
    for (auto &h : raws) h.decode_status=0;
  }
  void decoded(int decid, int rawid, int layer, int wire,
               double center, double initial, double pre) {
    if (!active || event < 0 || decpos.count(decid)) return;
    DecodedHitsRow h; h.run=run; h.event=event;
    h.dec_id=decid; h.raw_id=rawid; h.layer=layer; h.wire=wire;
    h.wire_center=center; h.drift_time_initial=initial;
    h.drift_time_pre=pre; h.tof_pass=-1;
    decpos[decid]=decs.size(); decs.push_back(h);
    auto it=rawpos.find(rawid);
    if(it!=rawpos.end()) {
      raws[it->second].dec_id=decid;
      raws[it->second].decode_status=1;
    }
  }
  void tof(int decid, int pass) {
    auto it=decpos.find(decid);
    if(it!=decpos.end()) decs[it->second].tof_pass=pass;
  }
  void drift(int decid, double time, double distance) {
    auto it=decpos.find(decid);
    if(it!=decpos.end()) {
      decs[it->second].drift_time=time;
      decs[it->second].drift_distance=distance;
    }
  }
  void space(int id, int chamber, double x, double y, int nhits) {
    if (!active || event < 0 || id <= 0 || sppos.count(id)) return;
    SpacePointsRow row; row.run=run; row.event=event;
    row.sp_id=id; row.chamber=chamber; row.x_sp=x; row.y_sp=y;
    row.n_hits=nhits;
    sppos[id]=spaces.size(); spaces.push_back(std::move(row));
  }
  void spaceNHit(int id, int nhits) {
    auto it=sppos.find(id);
    if(it!=sppos.end()) spaces[it->second].n_hits=nhits;
  }
  void spaceSelected(int id, int pass) {
    auto it=sppos.find(id);
    if(it!=sppos.end()) spaces[it->second].sp_selected=pass;
  }
  void spacePrecut(int id, int pass) {
    auto it=sppos.find(id);
    if(it!=sppos.end()) spaces[it->second].precut_pass=pass;
  }
  void hitset(int id, int spid, int nhits, const int32_t* ids) {
    if (!active || event < 0 || id <= 0 || hitsetpos.count(id)) return;
    auto it=sppos.find(spid);
    if(it==sppos.end() || nhits<=0 || nhits>50) return;
    HitSetsRow row; row.run=run; row.event=event;
    row.hitset_id=id; row.sp_id=spid; row.n_hits=nhits;
    row.hit_ids.assign(ids,ids+nhits);
    hitsetpos[id]=hitsets.size();
    spHitsets[spid].push_back(hitsets.size());
    hitsets.push_back(std::move(row));
    auto &sp=spaces[it->second];
    if(sp.n_hitsets<0) sp.n_hitsets=0;
    ++sp.n_hitsets;
  }
  void lrHyp(int spid, int alt, int nhits, const float* signs,
             int fitok, const float* stub, float chi2) {
    if (!active || event < 0 || nhits < 1 || nhits > 50) return;
    auto ss=spHitsets.find(spid);
    if (ss==spHitsets.end() || alt<1 || size_t(alt)>ss->second.size()) return;
    auto hi=ss->second[size_t(alt-1)];
    auto &hs=hitsets[hi];
    LRHypothesesRow lr;
    lr.run=run; lr.event=event; lr.lr_id=++next_lr_id;
    lr.sp_id=spid; lr.hitset_id=hs.hitset_id;
    lr.n_hits=nhits; lr.lr_mask=0;
    for(int i=0;i<nhits;i++) if(signs[i]>0) lr.lr_mask|=(int64_t(1)<<i);
    lr.is_best_in_hitset=0; lr.is_best_in_sp=0;
    if(hs.n_lr_generated<0) hs.n_lr_generated=0;
    ++hs.n_lr_generated;
    auto spIt=sppos.find(spid);
    if(spIt!=sppos.end()) {
      auto &sr=spaces[spIt->second];
      if(sr.n_lr_generated<0) sr.n_lr_generated=0;
      ++sr.n_lr_generated;
    }
    if(fitok) {
      ++n_lr_fit_attempts;
      if(hs.n_lr_fitted<0) hs.n_lr_fitted=0;
      ++hs.n_lr_fitted;
      if(spIt!=sppos.end()) {
        auto &sr=spaces[spIt->second];
        if(sr.n_lr_fitted<0) sr.n_lr_fitted=0;
        ++sr.n_lr_fitted;
      }
      lr.chi2=chi2; lr.ndf=nhits-3;
      lr.fit_x=stub[0]; lr.fit_y=stub[1];
      lr.fit_xp=stub[2]; lr.fit_yp=stub[3];
      // The original Fortran chooses the first strictly smaller chi2.
      if(std::isfinite(chi2) && chi2<1.e10 &&
         (hs.best_lr_id<0 || chi2<hs.best_lr_chi2)) {
        if(hs.best_lr_id>0)
          lrs[size_t(hs.best_lr_id-1)].is_best_in_hitset=0;
        hs.best_lr_id=lr.lr_id; hs.best_lr_chi2=chi2;
        lr.is_best_in_hitset=1;
      }
    }
    lrs.push_back(std::move(lr));
  }
  void lrFinish(int spid, int alt, double x, double y,
                double xp, double yp) {
    auto ss=spHitsets.find(spid);
    auto spIt=sppos.find(spid);
    if(ss==spHitsets.end() || spIt==sppos.end() || alt<1 ||
       size_t(alt)>ss->second.size()) return;
    auto &hs=hitsets[ss->second[size_t(alt-1)]];
    if(hs.best_lr_id<0) return;
    auto &sp=spaces[spIt->second];
    sp.best_hitset_id=hs.hitset_id;
    sp.best_lr_id=hs.best_lr_id;
    sp.stub_x=x; sp.stub_y=y; sp.stub_xp=xp; sp.stub_yp=yp;
    sp.stub_chi2=hs.best_lr_chi2;
    hs.is_selected=1;
    lrs[size_t(hs.best_lr_id-1)].is_best_in_sp=1;
    // Every unselected hitset of this SP stays explicitly unselected.
    for(auto i:ss->second) if(hitsets[i].hitset_id!=hs.hitset_id)
       hitsets[i].is_selected=0;
  }
  void stubLink(int id, int sp1, int sp2, double dx, double dy,
                double dxp, double dyp, int pass) {
    if (!active || event<0 || id<=0) return;
    StubLinksRow r; r.run=run;r.event=event;r.link_id=id;
    r.sp1_id=sp1;r.sp2_id=sp2;
    auto i=sppos.find(sp1),j=sppos.find(sp2);
    if(i!=sppos.end()) r.lr1_id=spaces[i->second].best_lr_id;
    if(j!=sppos.end()) r.lr2_id=spaces[j->second].best_lr_id;
    r.dx=dx;r.dy=dy;r.dxp=dxp;r.dyp=dyp;r.link_pass=pass;
    links.push_back(std::move(r));
  }
  void trackSnapshot(int id,int parent,int link,int nsp,const int32_t* spids) {
    if(!active || event<0 || id<=0 || nsp<1 || nsp>500) return;
    auto it=trackpos.find(id);
    if(it==trackpos.end()) {
      TracksRow r;r.run=run;r.event=event;r.track_id=id;
      if(parent>0) {
        auto p=trackpos.find(parent);
        if(p!=trackpos.end()) r.link_ids=tracks[p->second].link_ids;
      }
      trackpos[id]=tracks.size();tracks.push_back(std::move(r));
      it=trackpos.find(id);
    }
    auto &r=tracks[it->second];
    r.sp_ids.assign(spids,spids+nsp);
    r.n_sp=nsp;
    if(link>0) r.link_ids.push_back(link);
  }
  void trackHit(int id,int dec,int spid,int layer,int wire,double drift) {
    auto it=trackpos.find(id);
    if(it==trackpos.end()) return;
    TrackHitsRow h;h.run=run;h.event=event;h.track_id=id;
    h.dec_id=dec;h.sp_id=spid;h.layer=layer;h.wire=wire;
    h.drift_distance=drift; h.lr_sign=0;
    auto si=sppos.find(spid);
    if(si!=sppos.end()) {
      auto best=spaces[si->second].best_lr_id;
      if(best>0 && size_t(best)<=lrs.size()) {
        const auto &lr=lrs[size_t(best-1)];
        auto hi=hitsetpos.find(lr.hitset_id);
        if(hi!=hitsetpos.end()) {
          const auto &ids=hitsets[hi->second].hit_ids;
          for(size_t k=0;k<ids.size();++k) if(ids[k]==dec) {
            h.lr_sign=((lr.lr_mask>>k)&1)?1:-1;break;
          }
        }
      }
    }
    trackhitpos[id].push_back(trackhits.size());
    trackhits.push_back(std::move(h));
    tracks[it->second].n_hits=int32_t(trackhitpos[id].size());
  }
  void trackHitFit(int id,int occurrence,double wire,double pred,
                   double drift,double sigma) {
    auto it=trackhitpos.find(id);
    if(it==trackhitpos.end() || occurrence<1 ||
       size_t(occurrence)>it->second.size()) return;
    auto &h=trackhits[it->second[size_t(occurrence-1)]];
    h.wire_coord=wire;h.track_coord=pred;h.residual=wire-pred;
    h.drift_distance=drift;h.sigma=sigma;
    // A decoded hit shared by several SPs can be overwritten in the
    // linker's (track,hit) wire-coordinate bank. Record the LR sign
    // actually used by this FP fit, rather than the earlier SP sign.
    auto d=decpos.find(h.dec_id);
    if(d!=decpos.end() && std::isfinite(wire) &&
       std::isfinite(decs[d->second].wire_center) &&
       std::isfinite(drift) && std::fabs(drift)>1.e-8) {
       h.lr_sign=(wire>=decs[d->second].wire_center)?1:-1;
    }
  }
  void trackFitAttempt(int id) {
    if(trackpos.count(id)) ++n_track_fit_attempts;
  }
  void trackFit(int id,double x,double y,double xp,double yp,
                double chi2,int ndf) {
    auto it=trackpos.find(id);
    if(it==trackpos.end()) return;
    auto &r=tracks[it->second];
    r.x_fp=x;r.y_fp=y;r.xp_fp=xp;r.yp_fp=yp;
    r.chi2=chi2;r.ndf=ndf;
    if(ndf>0) r.chi2_ndf=chi2/double(ndf);
  }
  void trackFinal(int id,int finalid) {
    auto it=trackpos.find(id);
    if(it!=trackpos.end()) tracks[it->second].final_track_id=finalid;
  }
  void end(const int32_t* sp, int lrgen, int lrfit, int links,
           int finalTracks, int tofstat) {
    if (!active || event < 0) return;
    (void)lrgen; (void)lrfit; (void)links; // counters from recorded rows
    for (const auto &h : raws)
      if (h.layer >= 1 && h.layer <= 12) ++ev.n_raw[h.layer-1];
    for (const auto &h : decs)
      if (h.layer >= 1 && h.layer <= 12) ++ev.n_decoded[h.layer-1];
    ev.n_sp[0]=sp[0]; ev.n_sp[1]=sp[1];
    ev.n_hitsets=static_cast<int32_t>(hitsets.size());
    ev.n_lr_generated=int32_t(lrs.size());
    ev.n_lr_fitted=n_lr_fit_attempts;
    ev.n_links=int32_t(this->links.size());
    ev.n_tracks_generated=int32_t(tracks.size());
    ev.n_tracks_fitted=n_track_fit_attempts;
    ev.n_tracks_final=finalTracks;
    ev.tof_cut_status = tofstat == 2 ? 2 : (tofstat==1 ? 1 : 0);
    bEvents=ev; tEvents->Fill();
    for (auto &h : raws) { bRawHits=h; tRawHits->Fill(); }
    for (auto &h : decs) { bDecodedHits=h; tDecodedHits->Fill(); }
    for (auto &s : spaces) { bSpacePoints=s; tSpacePoints->Fill(); }
    for (auto &h : hitsets) { bHitSets=h; tHitSets->Fill(); }
    for (auto &lr : lrs) { bLRHypotheses=lr; tLRHypotheses->Fill(); }
    for (auto &link : this->links) { bStubLinks=link; tStubLinks->Fill(); }
    for (auto &track : tracks) { bTracks=track; tTracks->Fill(); }
    for (auto &hit : trackhits) { bTrackHits=hit; tTrackHits->Fill(); }
    clear();
  }
  void close() {
    if(!active) return;
    file->cd(); file->Write("",TObject::kOverwrite);
    file->Close(); file.reset(); active=false; clear();
  }
};
Writer writer;
} // namespace

extern "C" {
void htrk_open_(const int32_t* run, int32_t* status) {
  *status=1;
  try {
    writer.close();
    char filename[256];
    std::snprintf(filename,sizeof(filename),"rootfiles/htracking%d.root",*run);
    writer.file.reset(TFile::Open(filename,"RECREATE"));
    if(!writer.file || writer.file->IsZombie()) { writer.file.reset(); return; }
    writer.active=true; writer.book(); writer.clear(); *status=0;
  } catch (...) { writer.close(); }
}
void htrk_close_() { writer.close(); }
void htrk_clear_() { writer.clear(); }
void htrk_begin_(const int32_t* run, const int32_t* event, const int32_t* type) {
  writer.begin(*run,*event,*type);
}
void htrk_raw_(const int32_t* id, const int32_t* layer, const int32_t* wire, const int32_t* tdc) {
  writer.raw(*id,*layer,*wire,*tdc);
}
void htrk_decode_started_() { writer.decodeStarted(); }
void htrk_decoded_(const int32_t* dec, const int32_t* raw, const int32_t* layer,
                   const int32_t* wire, const float* center, const float* initial,
                   const float* pre) {
  writer.decoded(*dec,*raw,*layer,*wire,*center,*initial,*pre);
}
void htrk_mark_tof_(const int32_t* dec, const int32_t* pass) { writer.tof(*dec,*pass); }
void htrk_drift_(const int32_t* dec, const float* time, const float* distance) {
  writer.drift(*dec,*time,*distance);
}
void htrk_space_point_(const int32_t* id, const int32_t* chamber,
                       const float* x, const float* y, const int32_t* nhits) {
  writer.space(*id,*chamber,*x,*y,*nhits);
}
void htrk_space_nhits_(const int32_t* id,const int32_t* nhits) {
  writer.spaceNHit(*id,*nhits);
}
void htrk_space_selected_(const int32_t* id,const int32_t* pass) {
  writer.spaceSelected(*id,*pass);
}
void htrk_space_precut_(const int32_t* id,const int32_t* pass) {
  writer.spacePrecut(*id,*pass);
}
void htrk_hitset_(const int32_t* id,const int32_t* sp,
                  const int32_t* nhits,const int32_t* ids) {
  writer.hitset(*id,*sp,*nhits,ids);
}
void htrk_lr_hyp_(const int32_t* sp,const int32_t* alt,
                  const int32_t* nhits,const float* signs,
                  const int32_t* fitok,const float* stub,const float* chi2) {
  writer.lrHyp(*sp,*alt,*nhits,signs,*fitok,stub,(*fitok?*chi2:0.f));
}
void htrk_lr_finish_(const int32_t* sp,const int32_t* alt,
                     const float* x,const float* y,
                     const float* xp,const float* yp) {
  writer.lrFinish(*sp,*alt,*x,*y,*xp,*yp);
}
void htrk_stub_link_(const int32_t* id,const int32_t* sp1,const int32_t* sp2,
                     const float* dx,const float* dy,
                     const float* dxp,const float* dyp,const int32_t* pass) {
  writer.stubLink(*id,*sp1,*sp2,*dx,*dy,*dxp,*dyp,*pass);
}
void htrk_track_snapshot_(const int32_t* id,const int32_t* parent,
                          const int32_t* link,const int32_t* nsp,
                          const int32_t* spids) {
  writer.trackSnapshot(*id,*parent,*link,*nsp,spids);
}
void htrk_track_hit_(const int32_t* id,const int32_t* dec,
                     const int32_t* sp,const int32_t* layer,
                     const int32_t* wire,const float* drift) {
  writer.trackHit(*id,*dec,*sp,*layer,*wire,*drift);
}
void htrk_track_hit_fit_(const int32_t* id,const int32_t* occurrence,
                         const float* coord,const float* trackcoord,
                         const float* drift,const float* sigma) {
  writer.trackHitFit(*id,*occurrence,*coord,*trackcoord,*drift,*sigma);
}
void htrk_track_fit_attempt_(const int32_t* id) {
  writer.trackFitAttempt(*id);
}
void htrk_track_fit_(const int32_t* id,const float* x,const float* y,
                     const float* xp,const float* yp,
                     const double* chi2,const int32_t* ndf) {
  writer.trackFit(*id,*x,*y,*xp,*yp,*chi2,*ndf);
}
void htrk_track_final_(const int32_t* id,const int32_t* finalid) {
  writer.trackFinal(*id,*finalid);
}
void htrk_end_(const int32_t* sp, const int32_t* lrgen, const int32_t* lrfit,
               const int32_t* links, const int32_t* finalTracks, const int32_t* tofstat) {
  writer.end(sp,*lrgen,*lrfit,*links,*finalTracks,*tofstat);
}
} // extern C
