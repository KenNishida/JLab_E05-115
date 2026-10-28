#include <TFile.h>
#include <TTree.h>
#include <TString.h>
#include <TSystem.h>

#include <map>
#include <set>
#include <utility>
#include <iostream>
#include <cmath>

// Join raw and stage events by (run,event), never by entry number.
// Both input ROOT files must come from the same replay and event range.
void make_hsummary(const char* rawname="../replay/e05115replay/rootfiles/hraw76312.root",
                   const char* stagename="../replay/e05115replay/rootfiles/hstage76312.root",
                   const char* outname="hsummary76312.root")
{
    TFile rawFile(rawname,"READ"), stageFile(stagename,"READ");
    if (rawFile.IsZombie() || stageFile.IsZombie()) {
        std::cerr << "Failed to open one of the input files" << std::endl;
        return;
    }
    TTree *raw=nullptr, *stage=nullptr;
    rawFile.GetObject("h9030",raw);
    stageFile.GetObject("h9040",stage);
    if (!raw || !stage) {
        std::cerr << "Missing h9030 or h9040" << std::endl;
        return;
    }

    Float_t rr=0, re=0, sr=0, se=0;
    raw->SetBranchAddress("run",&rr);
    raw->SetBranchAddress("event",&re);
    stage->SetBranchAddress("run",&sr);
    stage->SetBranchAddress("event",&se);

    using Key = std::pair<int,int>;
    std::map<Key,Long64_t> stageIndex;
    for (Long64_t i=0;i<stage->GetEntries();++i) {
        stage->GetEntry(i);
        const Key key{(int)std::lround(sr),(int)std::lround(se)};
        if (!stageIndex.emplace(key,i).second) {
            std::cerr << "Duplicate event in h9040: " << key.first
                      << ":" << key.second << std::endl;
            return;
        }
    }

    std::map<Key,Long64_t> rawIndex;
    for (Long64_t i=0;i<raw->GetEntries();++i) {
        raw->GetEntry(i);
        const Key key{(int)std::lround(rr),(int)std::lround(re)};
        if (!rawIndex.emplace(key,i).second) {
            std::cerr << "Duplicate event in h9030: " << key.first
                      << ":" << key.second << std::endl;
            return;
        }
        if (!stageIndex.count(key)) {
            std::cerr << "Missing stage event " << key.first << ":"
                      << key.second << std::endl;
            return;
        }
    }
    if (rawIndex.size()!=stageIndex.size()) {
        std::cerr << "Event sets differ: raw=" << rawIndex.size()
                  << " stage=" << stageIndex.size() << std::endl;
        return;
    }

    Float_t revtype=0, rrawtot=0, rkdc1=0, rkdc2=0;
    Float_t rdecstat=0, rdectot=0, rnfp=0, rnphys=0;
    Float_t rplane[12]={}, dplane[12]={};
    raw->SetBranchAddress("evtype",&revtype);
    raw->SetBranchAddress("rawtot",&rrawtot);
    raw->SetBranchAddress("kdc1",&rkdc1);
    raw->SetBranchAddress("kdc2",&rkdc2);
    raw->SetBranchAddress("decstat",&rdecstat);
    raw->SetBranchAddress("dectot",&rdectot);
    raw->SetBranchAddress("nfp",&rnfp);
    raw->SetBranchAddress("nphys",&rnphys);
    for(int i=0;i<12;++i) {
        raw->SetBranchAddress(Form("raw%02d",i+1),&rplane[i]);
        raw->SetBranchAddress(Form("dec%02d",i+1),&dplane[i]);
    }

    Float_t stofstat=0, spre=0, spost=0, sspstat=0, slrstat=0, slinkstat=0;
    Float_t sspraw1=0, sspraw2=0, ssp1=0, ssp2=0;
    Float_t slrplan=0, slrfit=0, slinkall=0, slinkeval=0, slinkpass=0;
    Float_t snfp=0, snphys=0, spreplane[12]={}, spostplane[12]={};
    stage->SetBranchAddress("tofstat",&stofstat);
    stage->SetBranchAddress("pretotal",&spre);
    stage->SetBranchAddress("posttot",&spost);
    stage->SetBranchAddress("spstat",&sspstat);
    stage->SetBranchAddress("lrstat",&slrstat);
    stage->SetBranchAddress("linkstat",&slinkstat);
    stage->SetBranchAddress("spraw1",&sspraw1);
    stage->SetBranchAddress("spraw2",&sspraw2);
    stage->SetBranchAddress("sp1",&ssp1);
    stage->SetBranchAddress("sp2",&ssp2);
    stage->SetBranchAddress("lrplan",&slrplan);
    stage->SetBranchAddress("lrfit",&slrfit);
    stage->SetBranchAddress("linkall",&slinkall);
    stage->SetBranchAddress("linkeval",&slinkeval);
    stage->SetBranchAddress("linkpass",&slinkpass);
    stage->SetBranchAddress("nfp",&snfp);
    stage->SetBranchAddress("nphys",&snphys);
    for(int i=0;i<12;++i) {
        stage->SetBranchAddress(Form("pre%02d",i+1),&spreplane[i]);
        stage->SetBranchAddress(Form("post%02d",i+1),&spostplane[i]);
    }

    // A compact 1-event/entry view. The input files remain independent.
    TFile outFile(outname,"RECREATE");
    if (outFile.IsZombie()) {
        std::cerr << "Cannot create " << outname << std::endl;
        return;
    }
    TTree summary("event_summary","Joined raw and tracking-stage diagnostics");
    Float_t run=0,event=0,evtype=0,rawtot=0,kdc1=0,kdc2=0,decstat=0,dectot=0;
    Float_t pretotal=0,posttot=0,tofstat=0,spstat=0,lrstat=0,linkstat=0;
    Float_t spraw1=0,spraw2=0,sp1=0,sp2=0,lrplan=0,lrfit=0;
    Float_t linkall=0,linkeval=0,linkpass=0,nfp=0,nphys=0;
    Float_t rawplane[12],decplane[12],preplane[12],postplane[12];
#define BR(x) summary.Branch(#x,&x,#x "/F")
    BR(run); BR(event); BR(evtype); BR(rawtot); BR(kdc1); BR(kdc2);
    BR(decstat); BR(dectot); BR(pretotal); BR(posttot); BR(tofstat);
    BR(spstat); BR(lrstat); BR(linkstat); BR(spraw1); BR(spraw2);
    BR(sp1); BR(sp2); BR(lrplan); BR(lrfit); BR(linkall);
    BR(linkeval); BR(linkpass); BR(nfp); BR(nphys);
#undef BR
    summary.Branch("rawplane",rawplane,"rawplane[12]/F");
    summary.Branch("decplane",decplane,"decplane[12]/F");
    summary.Branch("preplane",preplane,"preplane[12]/F");
    summary.Branch("postplane",postplane,"postplane[12]/F");

    Long64_t ntrackMismatch=0,npreMismatch=0,nbadTof=0,nbadCombinations=0;
    for(const auto& entry : rawIndex) {
        raw->GetEntry(entry.second);
        stage->GetEntry(stageIndex.at(entry.first));
        run=rr; event=re; evtype=revtype;
        rawtot=rrawtot; kdc1=rkdc1; kdc2=rkdc2;
        decstat=rdecstat; dectot=rdectot;
        pretotal=spre; posttot=spost; tofstat=stofstat;
        spstat=sspstat; lrstat=slrstat; linkstat=slinkstat;
        spraw1=sspraw1; spraw2=sspraw2; sp1=ssp1; sp2=ssp2;
        lrplan=slrplan; lrfit=slrfit;
        linkall=slinkall; linkeval=slinkeval; linkpass=slinkpass;
        nfp=rnfp; nphys=rnphys;
        for(int j=0;j<12;++j) {
            rawplane[j]=rplane[j]; decplane[j]=dplane[j];
            preplane[j]=spreplane[j]; postplane[j]=spostplane[j];
        }
        if(rnfp!=snfp || rnphys!=snphys) ++ntrackMismatch;
        if(stofstat>=1 && rdecstat==1 && rdectot!=spre) ++npreMismatch;
        if(stofstat==2 && (spost>spre || spost<0)) ++nbadTof;
        if(slrfit>slrplan || slinkpass>slinkeval) ++nbadCombinations;
        summary.Fill();
    }
    if(ntrackMismatch || nbadTof || nbadCombinations) {
        std::cerr << "FAILED QA: track mismatch=" << ntrackMismatch
                  << ", TOF inconsistency=" << nbadTof
                  << ", combination inconsistency=" << nbadCombinations
                  << std::endl;
        outFile.Close();
        gSystem->Unlink(outname);
        return;
    }
    summary.Write();
    outFile.Close();
    std::cout << "Joined " << rawIndex.size() << " events -> " << outname
              << "; decoded/preTOF mismatches=" << npreMismatch << std::endl;
}
