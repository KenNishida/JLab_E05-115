/*
  HKSAnalysis.cc

  M.Kawai
*/

#include "HKSAnalysis.hh"
#include "HKSTargetHit.hh"
#include "HKSVDHit.hh"
#include "HKSTOFHit.hh"
#include "HKSACHit.hh"
#include "HKSWCHit.hh"
#include "HKSDCHit.hh"
#include "HKSSSHit.hh"

#include "HKSTrajectory.hh"
#include "HKSSteppingAction.hh"
#include "RootHelper.hh"

#include "HKSParamMan.hh"

//#include "HYCS.hh"

#include "G4Run.hh"
#include "G4Event.hh"
#include "G4Step.hh"
#include "G4String.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4ThreeVector.hh"
#include "G4Poisson.hh"

#include "Randomize.hh"
#include <CLHEP/Random/RandGaussQ.h>
#include <CLHEP/Random/RandGauss.h>

#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <string>
#include <vector>

//#ifdef USE_CSTREAM
#include <cstdio>
//#endif

G4double PI = 4.*atan(1.);
//TFile *f;

///////////////////////////////////////
HKSAnalysis::HKSAnalysis()
  : fTriggered(false), fActive_(true), fOutput(false)
///////////////////////////////////////
{
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  filename_ = paramMan->GetHbookFileName();

  defineHistograms();
}

///////////////////////////
HKSAnalysis::~HKSAnalysis()
///////////////////////////
{
  SaveFile();
}


//////////////////////////////////////////////////
void HKSAnalysis::BeginOfRun( const G4Run *)
//////////////////////////////////////////////////
{
  trigNum=0;
  //HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  //filename_ = paramMan->GetHbookFileName();
  //f = new TFile(filename_,"recreate");
  //tree = new TTree("tree","tree of HKS Geant4 Simulation");
  //defineHistograms(tree);
  //G4cout<<"BeginOfRun"<<G4endl;
}

////////////////////////////////////////////////
void HKSAnalysis::EndOfRun( const G4Run *aRun )
////////////////////////////////////////////////
{
  G4cout << "#events generated = " << aRun->GetNumberOfEvent() << G4endl;
  G4cout << "#events triggered = " << trigNum << G4endl;
  //G4cout<<"EndOfRun"<<G4endl;
  //tree->Write(); 
  //f->Write();
}

/////////////////////////////////////////////////////////
void HKSAnalysis::BeginOfEvent( const G4Event *)
/////////////////////////////////////////////////////////
{
  //G4cout << "Begin Of Event" <<G4endl;
}

////////////////////////////////////////////////////////////////
void HKSAnalysis::PrimaryGeneration( const G4ThreeVector &pos,
				     const G4ThreeVector &mom,
				     const G4int ID)
////////////////////////////////////////////////////////////////
{
  //G4cout << "Primary Generation" <<G4endl;
  gPos_=pos; gMom_=mom;
  evID = ID;
}

////////////////////////////////////////////////////////
void HKSAnalysis::EndOfEvent( const G4Event *anEvent  )
////////////////////////////////////////////////////////
{
  G4int eventID= anEvent->GetEventID()+1;
  if(eventID%1000 ==0){
    G4cout << "Event Number :" << std::setw(6) << eventID <<G4endl;
    G4cout << "Triggered Event :" << std::setw(6) << trigNum <<G4endl;
  }
  fTriggered = false;
  NormalAnalysis( anEvent );
  //G4cout << "End Of Event" <<G4endl;
}

///////////////////////////////////////////////////////////
void HKSAnalysis::NormalAnalysis( const G4Event *anEvent )
///////////////////////////////////////////////////////////
{
  TTree *tree = dynamic_cast<TTree *>(gFile->Get("tree"));
  InitializeEvent();
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  int flag = paramMan->GetEventFlag();
  int KDCFlag = paramMan->GetKDCFlag();
  
  G4HCofThisEvent *HCE = anEvent->GetHCofThisEvent();
  G4SDManager *SDMan = G4SDManager::GetSDMpointer();
  //  G4int colIdTarget = SDMan->GetCollectionID( "TargetCollection" );
  G4int colIdVD[15];
  G4double pt  = -1000.;G4double pxt = -1000.;
  G4double pyt = -1000.;G4double pzt = -1000.;
  G4double theta = -1000.;G4double phi   = -1000.;
  G4double xpt = -1000.;G4double ypt = -1000.;
  G4double xt = -1000., yt = -1000., zt = -1000.;
  G4double VDEdep = 0.;
  G4double xVD=-1000.; G4double yVD=-1000.; G4double zVD=-1000.; 
  G4double xvVD=-1000.;G4double yvVD=-1000.;G4double zvVD=-1000.;
  G4double pxVD=-1000.;G4double pyVD=-1000.;G4double pzVD=-1000.;
  G4double pVD=-1000.; G4double xpVD=-1000.;G4double ypVD=-1000.;
  TargetEdep = 0.0;
  G4double TOFEdep = 0.0;
  G4int VDhit[16];

  HKSVDHitsCollection *VDHC[15];
  
  //=======SDetector========//

  G4int colIdTarget = SDMan->GetCollectionID( "TargetCollection" );
  //G4int colIdTD[8];
  //for( int i=0 ; i<8; ++i){
  // std::ostringstream id;
  //  id << i;
  //  G4String Name = G4String("TD")+id.str().c_str()+"-Collection";
  //  colIdTD[i] = SDMan-> GetCollectionID( Name );
  // }

  //G4int colIdTOF = SDMan->GetCollectionID( "TOFCollection" );
 
  G4int colIdTOF[3];
  for( int i=0; i<3; i++ ){
    std::ostringstream Tid;
    Tid << i;
    G4String TName = G4String("TOF")+Tid.str().c_str()+"-Collection";
    colIdTOF[i] = SDMan->GetCollectionID( TName );
  }
  
  
  G4int colIdAC[3];
  for( int i=0; i<3; i++ ){
    std::ostringstream Aid;
    Aid << i;
    G4String AName = G4String("AC")+Aid.str().c_str()+"-Collection";
    colIdAC[i] = SDMan->GetCollectionID( AName );
  }
  
  G4int colIdWC[2];
  for( int i=0; i<2; i++ ){
    std::ostringstream Wid;
    Wid << i;
    G4String WName = G4String("WC")+Wid.str().c_str()+"-Collection";
    colIdWC[i] = SDMan->GetCollectionID( WName );
  }


  G4int AChit[3], WChit[2];
  G4int DChit[2];
  G4int TOFhit[3];
  G4int TDhit[8];
  
  for( int i=0; i<3; i++ ) TOFhit[i] = TOFpid[i] = 0;
  for( int i=0; i<3; i++ ) AChit[i] = ACpid[i] = 0;
  for( int i=0; i<2; i++ ) WChit[i] = WCpid[i] = 0;
  for( int i=0; i<14; i++ ) VDhit[i] = VDpid[i] = 0;
  for( int i=0; i<8; i++ ) TDhit[i] = 0;
  for( int i=0; i<2; i++ ) DChit[i] = 0;
  for( int i=0; i<3; i++ ) ACseg[i] = TOFseg[i] =  0;
  for( int i=0; i<2; i++ ) WCseg[i] = 0;
    
  
   
  //=== Target ===//
  HKSTargetHitsCollection *TargetHC; 
  TargetHC = dynamic_cast<HKSTargetHitsCollection *>(HCE->GetHC(colIdTarget));
  if( TargetHC ){
    G4int nTargetHits = TargetHC->entries();
    for( G4int i=0; i<nTargetHits; i++ ){
      HKSTargetHit *aTargetHit = (*TargetHC)[i];
      TargetEdep += (aTargetHit->GetEdep())/(MeV);
      /*nHits = TargetHC->entries();
	for( G4int i=0; i<nHits; i++ ){
	HKSTargetHit *aHit = (*TargetHC)[i];
	TargetEdep += aHit->GetEdep();*/
    }
  } // if( HKSTargetHC )
  
  
  
  if (flag==0){
    //=========== TOF Counter =============//
    
    HKSTOFHitsCollection *tofHC[3];
    for( int i=0; i<3; ++i ){
      tofHC[i] = dynamic_cast<HKSTOFHitsCollection *>(HCE->GetHC(colIdTOF[i]));
      TOFhit[i] = -1;
      if( tofHC[i] ){
	G4int nTOFHits = tofHC[i]->entries();
	TOFhit[i] = nTOFHits;
	if( TOFhit[i] <= 0 ){
	  TOFseg[i] = -1;
	  for(int j=0; j<18; j++)
	    TOFtdc[i][j] = TOFadc[i][j] 	     
	      = TOFx[i][j] = TOFy[i][j] = -500.0;
	}
	else{
	  for( G4int j=0 ; j<nTOFHits ; j++ ){
	    HKSTOFHit *aTOFHit = (*tofHC[i])[j];
	    G4ThreeVector TOFpos = aTOFHit->GetLPos();
	    TOFseg[i] = aTOFHit->GetSegID();
	    TOFx[i][TOFseg[i]] = TOFpos.x()/cm;
	    TOFy[i][TOFseg[i]] = TOFpos.y()/cm;
	    G4double TOFtime = aTOFHit->GetTime()/ns;
	    TOFtdc[i][TOFseg[i]] =TOFtime;
	    TOFtdcSum[i] =TOFtdc[i][TOFseg[i]];
	    TOFEdep = (aTOFHit->GetEdep())/MeV;
	    TOFadc[i][TOFseg[i]] += TOFEdep;
	    TOFLength[i] = aTOFHit->GetLength()/cm;
	    G4String pname = aTOFHit->GetParticleName();
	    if(pname == "proton"){
	      TOFpid[i] = 1; 
	    }
	    else if(pname == "kaon+"){
	      TOFpid[i] = 2; 
	    }
	    else if(pname == "pi+"){
	      TOFpid[i] = 3;
	    }
	    else  TOFpid[i] = -1;
	  }
	}
      }
      else TOFtdc[i][TOFseg[i]] = TOFadc[i][TOFseg[i]] = -500.;
    }
    
    if( TOFhit[0]>0 && TOFhit[2]>0 ){TOFTrig = true;}
    else{TOFTrig=false;}
    if( TOFhit[0]>0 ){TOF1Trig = true;}
    else{TOF1Trig=false;}
    if( TOFhit[2]>0 ){TOF2Trig = true;}
    else{TOF2Trig=false;}
    
    //=========== Aerogel Cherenkov =============//
    G4double Ain = 1.05;
    //   G4double Arms = 5.0;
    
    HKSACHitsCollection *ACHC[3];
    for( int i=0; i<3; ++i ){
      ACHC[i] = dynamic_cast<HKSACHitsCollection *>(HCE->GetHC(colIdAC[i]));
      AChit[i] = -1;
      if( ACHC[i] ){
	G4int nACHits = ACHC[i]->entries();
	AChit[i] = nACHits;
	G4double ACnpe = 0.0;
	if( AChit[i] <= 0 ){
	  ACseg[i] = -1;
	  for( int j=0; j<7; j++)
	    ACtdc[i][j] = ACadc[i][j]        
	      = ACx[i][j] = ACy[i][j] = -500.0;
	}
	else{
	  for( G4int j=0 ; j<nACHits ; j++ ){
	    HKSACHit *aACHit = (*ACHC[i])[j];
	    G4ThreeVector ACpos = aACHit->GetLPos();
	    ACseg[i] = aACHit->GetSegID();
	    ACx[i][ACseg[i]] = ACpos.x()/cm;
	    ACy[i][ACseg[i]] = ACpos.y()/cm;
	    G4double ACbeta = aACHit->GetBeta();
	    G4String apname = aACHit->GetParticleName();
	    if(apname == "proton"){
	      ACpid[i] = 1; 
	    }
	    else if(apname == "kaon+"){
	      ACpid[i] = 2; 
	    }
	    else if(apname == "pi+"){
	      ACpid[i] = 3;
	    }
	    else  ACpid[i] = -1;
	    
	    if( ACbeta > 1/Ain ){
	      ACtdc[i][ACseg[i]] = aACHit->GetTime();
	      ACnpe = (aACHit->GetNpe());
	      ACadc[i][ACseg[i]] = G4Poisson(ACnpe);
	      ACadcSumLayer[i] =  ACadc[i][ACseg[i]] ;
	      ACsum =  ACadc[i][ACseg[i]] ;	 
	    }
	    else ACtdc[i][ACseg[i]] = ACadc[i][ACseg[i]] = -1.0;
	  }
	}
      }
    }
    
    if( AChit[0]>0 ) {AC1Trig = true;}
    else {AC1Trig=false;}
    if( AChit[1]>0 ) {AC2Trig = true;}
    else {AC2Trig=false;}
    if( AChit[2]>0 ) {AC3Trig = true;}
    else {AC3Trig=false;}
    
    
    //=========== Water Cherenkov =============//
    
    G4double Win = 1.33;
    //   G4double Wrms = 5.0;
    HKSWCHitsCollection *WCHC[2];
    for( int i=0; i<2; ++i ){
      WChit[i] = -1;
      WCHC[i] = dynamic_cast<HKSWCHitsCollection *>
	(HCE->GetHC(colIdWC[i]));
      if( WCHC[i] ){
	G4int nWCHits = WCHC[i]->entries();
	WChit[i] = nWCHits;
	G4double WCnpe = 0.0;
	if( WChit[i] <= 0 ){
	  for(int j= 0; j<12; j++){
	    WCadc[i][j] = -1.;
	    WCtdc[i][j] = WCadc[i][j] 
	      = WCx[i][j] = WCy[i][j] = WCz[i][j] = -500.0;
	  }
	}
	else{
	  for( G4int j=0; j<nWCHits; j++){
	    HKSWCHit *aWCHit = (*WCHC[i])[j];
	    WCseg[i] = aWCHit->GetSegID();
	    G4double WCbeta = aWCHit->GetBeta();
	    //G4double WClength = aWCHit->GetLength();
	    //	G4cout<<"length"<<WClength<<G4endl;
	    WCnpe = (aWCHit->GetNpe());
	    G4ThreeVector WCpos = aWCHit->GetLPos();
	    WCx[i][WCseg[i]] = WCpos.x()/cm;
	    WCy[i][WCseg[i]] = WCpos.y()/cm;
	    WCz[i][WCseg[i]] = WCpos.z()/cm;
	    G4String wpname = aWCHit->GetParticleName();
	    if(wpname == "proton"){
	      WCpid[i] = 1; 
	    }
	    else if(wpname == "kaon+"){
	      WCpid[i] = 2; 
	    }
	    else if(wpname == "pi+"){
	      WCpid[i] = 3;
	    }
	    else  WCpid[i] = -1;
	    
	    if( WCbeta > 1./Win ){
	      WCtdc[i][WCseg[i]] = aWCHit->GetTime();
	      WCadc[i][WCseg[i]] = G4Poisson(WCnpe);
	      WCadcSumLayer[i] = WCadc[i][WCseg[i]];
	      WCsum = WCadc[i][WCseg[i]];
	    }
	    else WCtdc[i][WCseg[i]] = WCadc[i][WCseg[i]] = -1.0;   
	  }
	}
      }
    }
    if( WChit[0]>0 ){WC1Trig = true;}
    else{WC1Trig=false;}
    if( WChit[1]>0 ){WC2Trig = true;}
    else{WC2Trig=false;}
    
  }//flag
  
  
  
  
  //=== Virtual Detector ===//
  for( int i=0 ; i<=14; ++i){
    std::ostringstream id;
    id << i;
    G4String Name = G4String("VD")+id.str().c_str()+"-Collection";
    colIdVD[i] = SDMan-> GetCollectionID( Name );
    VDHC[i] = dynamic_cast<HKSVDHitsCollection *>(HCE->GetHC(colIdVD[i]));
    if( VDHC[i] ){
      G4int nVHits = VDHC[i]->entries();
      G4ThreeVector lPos;
      G4ThreeVector lMom;
      G4ThreeVector Vertex;
      for( G4int j=0; j<nVHits; j++ ){
	HKSVDHit *aVHit = (*VDHC[i])[j];
	VDhit[i] = nVHits;
	xvVD = Vertex.x();
	yvVD = Vertex.y();
	zvVD = Vertex.z();
	VDEdep = aVHit->GetEdep();
	lPos = aVHit->GetPos();
	//lPos = aVHit->GetGPos();
	xVD = lPos.x();
	yVD = lPos.y();
	zVD = lPos.z();
	lMom = aVHit->GetMom();
	pxVD = lMom.x();
	pyVD = lMom.y();
	pzVD = lMom.z();
	pVD = sqrt(pxVD*pxVD+pyVD*pyVD+pzVD*pzVD);
	xpVD = atan2(pxVD, pzVD);
	ypVD = atan2(pyVD, pzVD);
	Vertex = aVHit->GetVertex();
	L[i] = aVHit->GettLength()/cm;
	P[i] =pVD/GeV;
	X[i] =xVD/cm;Y[i]=yVD/cm;Z[i]=zVD/cm;
	Xp[i]=xpVD/radian;Yp[i]=ypVD/radian;
	Xv[i]=xvVD;Yv[i]=yvVD;Zv[i]=zvVD;
	G4String vpname = aVHit->GetParticleName();
	if(vpname == "proton"){
	  VDpid[i] = 1; 
	}
	else if(vpname == "kaon+"){
	  VDpid[i] = 2; 
	}
	else if(vpname == "pi+"){
	  VDpid[i] = 3;
	}
	else  VDpid[i] = -1;
      }
    }//VDHC
  }
  
  if (KDCFlag!=0){
    // DriftChamber
    G4int colIdDC[2];
    HKSDCHitsCollection *DCHC[2];
    for( int i=0; i<2; i++ ){
      std::ostringstream Did;
      Did << i;
      G4String DName = G4String("DC")+Did.str().c_str()+"-Collection";
      colIdDC[i] = SDMan->GetCollectionID( DName );
      DCHC[i] = dynamic_cast<HKSDCHitsCollection *>(HCE->GetHC(colIdDC[i]));
      
      if( DCHC[i] ){
	G4int nHits = DCHC[i]->entries();
	G4ThreeVector lPos;
	G4ThreeVector lMom;
	for( G4int j=0; j<nHits; j++ ){
	  HKSDCHit *aHit = (*DCHC[i])[j];
	  G4int Layer = aHit->GetLayerID();
	  if( i == 0 ){
	    DC1hit[Layer]++;
	    lPos = aHit->GetLPos();
	    lMom = aHit->GetLMom();
	    KDC1X[Layer] = lPos.x()/cm;
	    KDC1Y[Layer] = lPos.y()/cm;
	    KDC1Z[Layer] = lPos.z()/cm;
	    G4double pxDC = lMom.x()/GeV;
	    G4double pyDC = lMom.y()/GeV;
	    G4double pzDC = lMom.z()/GeV;
	    KDC1P[Layer] = lMom.mag()/GeV;
	    KDC1Xp[Layer] = atan2(pxDC,pzDC)/radian;
	    KDC1Yp[Layer] = atan2(pyDC,pzDC)/radian;
	  } 
	  else if( i == 1 ){
	    DC2hit[Layer]++;
	    lPos = aHit->GetLPos();
	    lMom = aHit->GetLMom();
	    KDC2X[Layer] = lPos.x()/cm;
	    KDC2Y[Layer] = lPos.y()/cm;
	    KDC2Z[Layer] = lPos.z()/cm;
	    G4double pxDC = lMom.x()/GeV;
	    G4double pyDC = lMom.y()/GeV;
	    G4double pzDC = lMom.z()/GeV;
	    KDC2P[Layer] = lMom.mag()/GeV;
	    KDC2Xp[Layer] = atan2(pxDC,pzDC)/radian;
	    KDC2Yp[Layer] = atan2(pyDC,pzDC)/radian;
	  }
	}
      }
    }
  }//KDCFlag
  
  //G4cout<<VDhit[7]<<G4endl;
 
  xt  = gPos_.x();
  yt  = gPos_.y();
  zt  = gPos_.z();
  pt  = gMom_.mag();
  pxt = gMom_.x();
  pyt = gMom_.y();
  pzt = gMom_.z();
  theta = acos( pzt/pt );
  phi  = atan2( pyt, pxt );
  //xpt = atan2( pxt, pzt );
  //ypt = atan2( pyt, pzt );
  xpt = pxt/pzt;
  ypt = pyt/pzt;
  Mom=pt/GeV;
  Theta=theta/radian;
  Phi=phi/radian;
  Xpt=xpt/radian;Ypt=ypt/radian;
  Xt=xt/cm;Yt=yt/cm;Zt=zt/cm;

  if( VDhit[1]>0 && VDhit[2]>0 && VDhit[3]>0 
      && VDhit[4]>0 && VDhit[5]>0 && VDhit[6]>0){VD = true;}
  if( KDC1X[0]>-1000. && KDC1X[1]>-1000. && KDC1X[2]>-1000. 
      && KDC1X[3]>-1000. && KDC1X[4]>-1000. && KDC1X[5]>-1000. 
      && KDC2X[0]>-1000. && KDC2X[1]>-1000. && KDC2X[2]>-1000. 
      && KDC2X[3]>-1000. && KDC2X[4]>-1000. && KDC2X[5]>-1000.)
    {KDCTrig=true;} 
  if(VD){
    fTriggered = true;
  }
  if(TOFTrig && VD){
    Trig = true;
    trigNum++;
  }
  
  //if (fTriggered){ 
  //  tree->Fill();
  //}
  tree->Fill();// T.Gogami, 3/May/2012, for Solid angle estimation

}

//////////////////////////////////////////
void HKSAnalysis::InitializeEvent( void )
//////////////////////////////////////////
{
  Mom   = -1000.0;
  Theta = -1000.0;
  Phi   = -1000.0;
  Xpt   = -1000.0;
  Ypt   = -1000.0;
  KDCTrig=false;
  Trig=false;
  VD=false;
  TOFTrig=false;
  TOF1Trig=false;
  TOF2Trig=false;
  AC1Trig=false;
  AC2Trig=false;
  AC3Trig=false;
  WC1Trig=false;
  WC2Trig=false;
  for (int i=0;i<14;i++){
    X[i] =-1000.;
    Y[i] =-1000.;
    Z[i] =-1000.;
    Xp[i]=-1000.;
    Yp[i]=-1000.;
    P[i] =-1000.;
    L[i] =-1000.;
    Xv[i]=-1000.;
    Yv[i]=-1000.;
    Zv[i]=-1000.;
  }
  
  for (int i=0;i<6;i++){
    KDC1X[i] =-1000.;
    KDC1Y[i] =-1000.;
    KDC1Z[i] =-1000.;
    KDC1Xp[i]=-1000.;
    KDC1Yp[i]=-1000.;
    KDC1P[i] =-1000.;
  }
  for (int i=0;i<6;i++){
    KDC2X[i] =-1000.;
    KDC2Y[i] =-1000.;
    KDC2Z[i] =-1000.;
    KDC2Xp[i]=-1000.;
    KDC2Yp[i]=-1000.;
    KDC2P[i] =-1000.;
  }
  for(int i=0; i<3;i++){
    TOFLength[i]=0;
    for(int j=0; j<18;j++){
      //  TOFadc[i][j]=TOFtdc[i][j]=0.;
      //  TOFx[i][j]=TOFy[i][j]=-1000.;
      TOFadc[i][j]=0.0;
      TOFtdc[i][j]=0.0;
      TOFx[i][j]=-1000.0;
      TOFy[i][j]=-1000.0;
    }
  }
  for(int i=0; i<3;i++){
    for(int j=0; j<7;j++){
      //  ACadc[i][j]=ACtdc[i][j]=0.;
      //  ACx[i][j]=ACy[i][j]=-1000.;
      ACadc[i][j]=0.0;
      ACtdc[i][j]=0.0;
      ACx[i][j]=-1000.0;
      ACy[i][j]=-1000.0;
    }
  }
  for(int i=0; i<2;i++){
    for(int j=0; j<12;j++){
      //WCadc[i][j]=WCtdc[i][j]=0.;
      //WCx[i][j]=WCy[i][j]=-1000.;
      WCadc[i][j]=0.0;
      WCtdc[i][j]=0.0;
      WCx[i][j]=-1000.0;
      WCy[i][j]=-1000.0;
    }
  }
}

////////////////////////////////////////
void HKSAnalysis::SaveFile( void ) const
////////////////////////////////////////
{
 if( fActive_ )
   gFile->Write();
}

////////////////////////////////////////
void HKSAnalysis::Terminate( void ) const
////////////////////////////////////////
{
 if( fActive_ )
   gFile->Write();
   gFile->Close();
}

//////////////////////////////////////////////
void HKSAnalysis::outputData( const G4Event *)
//////////////////////////////////////////////
{
  /* if( fDummy ){
    fprintf( fDummy, "Kine: %5d %4.3f %4.3f %4.3f %7.3f %7.3f %7.3f \n",
	     evID, gPos_.x()/cm, gPos_.y()/cm, gPos_.z(),
	     gMom_.x()/MeV, gMom_.y()/MeV, gMom_.z()/MeV);
  }
   
  for( int i=0; i<2; ++i ){ 
  if( fDummy ){
  fprintf( fDummy, "%7s%2d %7.2f %6.3f %6.3f\n",
	   title.c_str(), WCseg[i], WCadc[i],
	   WCtdc[i]/ns, WCz[i]/cm );
    }
   }
  */
}

///////////////////////////////////////////
void HKSAnalysis::defineHistograms( void )
///////////////////////////////////////////
{
  new TFile( filename_, "recreate","",0 );
  //TTree *tree = (TTree *)(f->Get("tree"));
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  int flag = paramMan->GetEventFlag();
  int KDCFlag = paramMan->GetKDCFlag();//KDCFlag==0 -> No Chamber
  char sPKDC[100];
  char sXKDC[100];
  char sYKDC[100];
  char sZKDC[100];
  char sXpKDC[100];
  char sYpKDC[100];
  char sPKDCD[100];
  char sXKDCD[100];
  char sYKDCD[100];
  char sZKDCD[100];
  char sXpKDCD[100];
  char sYpKDCD[100];
  char sXWC[100];
  char sYWC[100];
  char sZWC[100];
  char sWCadc[100];
  char sWCtdc[100];
  char sXWCD[100];
  char sYWCD[100];
  char sZWCD[100];
  char sWCadcD[100];
  char sWCtdcD[100];
  char sXAC[100];
  char sYAC[100];
  char sACadc[100];
  char sACtdc[100];
  char sXACD[100];
  char sYACD[100];
  char sACadcD[100];
  char sACtdcD[100];
  char sXTOF[100];
  char sYTOF[100];
  char sTOFa[100];
  char sTOFt[100];
  char sXTOFD[100];
  char sYTOFD[100];
  char sTOFaD[100];
  char sTOFtD[100];
  
  HBTree("tree","tree of HKS");
  TTree *tree = dynamic_cast<TTree *>(gFile->Get("tree"));
  
  if( flag==0 || flag==1){ 
    tree->Branch("evID",     &evID, "evID/I"  );
    tree->Branch("KMom",     &Mom, "KMom/D"  );
    tree->Branch("KTheta",  &Theta, "KTheta/D");
    tree->Branch("KPhi",      &Phi, "KPhi/D"  );
    tree->Branch("KXpt",      &Xpt, "KXpt/D"  );
    tree->Branch("KYpt",      &Ypt, "KYpt/D"  );
    tree->Branch("KXt",      &Xt, "KXt/D"  );
    tree->Branch("KYt",      &Yt, "KYt/D"  );
    tree->Branch("KZt",      &Zt, "KZt/D"  );
    //tree->Branch("KXSp",    &X[0],  "KXSp/D");
    //tree->Branch("KYSp",    &Y[0],  "KYSp/D");
    //tree->Branch("KZSp",    &Z[0],  "KZSp/D");
    tree->Branch("KXQ1i",    &X[1],  "KXQ1i/D");
    tree->Branch("KYQ1i",    &Y[1],  "KYQ1i/D");
    tree->Branch("KZQ1i",    &Z[1],  "KZQ1i/D");
    tree->Branch("KXpQ1i",    &Xp[1],  "KXpQ1i/D");
    tree->Branch("KYpQ1i",    &Yp[1],  "KYpQ1i/D");
    tree->Branch("KpidQ1i",    &VDpid[1],  "KpidQ1i/I");
    tree->Branch("KXQ1e",    &X[2],  "KXQ1e/D");
    tree->Branch("KYQ1e",    &Y[2],  "KYQ1e/D");
    tree->Branch("KZQ1e",    &Z[2],  "KZQ1e/D");
    tree->Branch("KpidQ1e",    &VDpid[2],  "KpidQ1e/I");
    tree->Branch("KXQ2i",    &X[3],  "KXQ2i/D");
    tree->Branch("KYQ2i",    &Y[3],  "KYQ2i/D");
    tree->Branch("KZQ2i",    &Z[3],  "KZQ2i/D");
    tree->Branch("KpidQ2i",    &VDpid[3],  "KpidQ2i/I");
    tree->Branch("KXQ2e",    &X[4],  "KXQ2e/D");
    tree->Branch("KYQ2e",    &Y[4],  "KYQ2e/D");
    tree->Branch("KZQ2e",    &Z[4],  "KZQ2e/D");
    tree->Branch("KpidQ2e",    &VDpid[4],  "KpidQ2e/I");
    tree->Branch("KPDi",     &P[5],   "KPDi/D");//added by toshi
    tree->Branch("KXDi",     &X[5],   "KXDi/D");
    tree->Branch("KYDi",     &Y[5],   "KYDi/D");
    tree->Branch("KZDi",     &Z[5],   "KZDi/D");
    tree->Branch("KpidDi",    &VDpid[5],  "KpidDi/I");
    tree->Branch("KPDe",     &P[6],   "KPDe/D");//added by toshi
    tree->Branch("KXDe",     &X[6],   "KXDe/D");
    tree->Branch("KYDe",     &Y[6],   "KYDe/D");
    tree->Branch("KZDe",     &Z[6],   "KZDe/D");
    tree->Branch("KXpDe",  &Xp[6], "KXpDe/D");
    tree->Branch("KYpDe",  &Yp[6], "KYpDe/D");
    tree->Branch("KpidDe",    &VDpid[6],  "KpidDe/I");
    tree->Branch("KLFP1",    &L[7],  "KLFP1/D");
    tree->Branch("KPFP1",    &P[7],  "KPFP1/D");
    tree->Branch("KXFP1",    &X[7],  "KXFP1/D");
    tree->Branch("KYFP1",    &Y[7],  "KYFP1/D");
    tree->Branch("KZFP1",    &Z[7],  "KZFP1/D");
    tree->Branch("KXpFP1",  &Xp[7], "KXpFP1/D");
    tree->Branch("KYpFP1",  &Yp[7], "KYpFP1/D");
    tree->Branch("KXvFP1",  &Xv[7], "KXvFP1/D");
    tree->Branch("KYvFP1",  &Yv[7], "KYvFP1/D");
    tree->Branch("KZvFP1",  &Zv[7], "KZvFP1/D");
    tree->Branch("KpidFP1",    &VDpid[7],  "KpidFP1/I");
    tree->Branch("KLFP2",    &L[8],  "KLFP2/D");
    tree->Branch("KPFP2",    &P[8],  "KPFP2/D");
    tree->Branch("KXFP2",    &X[8],  "KXFP2/D");
    tree->Branch("KYFP2",    &Y[8],  "KYFP2/D");
    tree->Branch("KZFP2",    &Z[8],  "KZFP2/D");
    tree->Branch("KXpFP2",  &Xp[8], "KXpFP2/D");
    tree->Branch("KYpFP2",  &Yp[8], "KYpFP2/D");
    tree->Branch("KXvFP2",  &Xv[8], "KXvFP2/D");
    tree->Branch("KYvFP2",  &Yv[8], "KYvFP2/D");
    tree->Branch("KZvFP2",  &Zv[8], "KZvFP2/D");
    tree->Branch("KpidFP2",    &VDpid[8],  "KpidFP2/I");
    tree->Branch("KLFP3",    &L[9],  "KLFP3/D");
    tree->Branch("KPFP3",    &P[9],  "KPFP3/D");
    tree->Branch("KXFP3",    &X[9],  "KXFP3/D");
    tree->Branch("KYFP3",    &Y[9],  "KYFP3/D");
    tree->Branch("KZFP3",    &Z[9],  "KZFP3/D");
    tree->Branch("KXpFP3",  &Xp[9], "KXpFP3/D");
    tree->Branch("KYpFP3",  &Yp[9], "KYpFP3/D");
    tree->Branch("KXvFP3",  &Xv[9], "KXvFP3/D");
    tree->Branch("KYvFP3",  &Yv[9], "KYvFP3/D");
    tree->Branch("KZvFP3",  &Zv[9], "KZvFP3/D");
    tree->Branch("KpidFP3",    &VDpid[9],  "KpidFP3/I");
    tree->Branch("KLFP4",   &L[10],  "KLFP4/D");
    tree->Branch("KPFP4",   &P[10],  "KPFP4/D");
    tree->Branch("KXFP4",   &X[10],  "KXFP4/D");
    tree->Branch("KYFP4",   &Y[10],  "KYFP4/D");
    tree->Branch("KZFP4",   &Z[10],  "KZFP4/D");
    tree->Branch("KXpFP4", &Xp[10], "KXpFP4/D");
    tree->Branch("KYpFP4", &Yp[10], "KYpFP4/D");
    tree->Branch("KXvFP4", &Xv[10], "KXvFP4/D");
    tree->Branch("KYvFP4", &Yv[10], "KYvFP4/D");
    tree->Branch("KZvFP4", &Zv[10], "KZvFP4/D");
    tree->Branch("KpidFP4",    &VDpid[10],  "KpidFP4/I");
    tree->Branch("KLFP5",   &L[11],  "KLFP5/D");
    tree->Branch("KPFP5",   &P[11],  "KPFP5/D");
    tree->Branch("KXFP5",   &X[11],  "KXFP5/D");
    tree->Branch("KYFP5",   &Y[11],  "KYFP5/D");
    tree->Branch("KZFP5",   &Z[11],  "KZFP5/D");
    tree->Branch("KXpFP5", &Xp[11], "KXpFP5/D");
    tree->Branch("KYpFP5", &Yp[11], "KYpFP5/D");
    tree->Branch("KXvFP5", &Xv[11], "KXvFP5/D");
    tree->Branch("KYvFP5", &Yv[11], "KYvFP5/D");
    tree->Branch("KZvFP5", &Zv[11], "KZvFP5/D");
    tree->Branch("KpidFP5",    &VDpid[11],  "KpidFP5/I");
    tree->Branch("KXSp1",   &X[12],  "KXSp1/D");
    tree->Branch("KYSp1",   &Y[12],  "KYSp1/D");
    tree->Branch("KZSp1",   &Z[12],  "KZSp1/D");
    tree->Branch("XpSp1", &Xp[12], "XpSp1/D");
    tree->Branch("YpSp1", &Yp[12], "YpSp1/D");
    //tree->Branch("XvSp1", &Xv[12], "XvSp1/D");
    //tree->Branch("YvSp1", &Yv[12], "YvSp1/D");
    //tree->Branch("ZvSp1", &Zv[12], "ZvSp1/D");
    tree->Branch("KXSS",   &X[13],  "KXSS2/D");
    tree->Branch("KYSS",   &Y[13],  "KYSS2/D");
    tree->Branch("KZSS",   &Z[13],  "KZSS2/D");
    tree->Branch("KXpSS",  &Xp[13], "KXpSS2/D");
    tree->Branch("KYpSS",  &Yp[13], "KYpSS2/D");
    //tree->Branch("XvSp2", &Xv[13], "XvSp2/D");
    //tree->Branch("YvSp2", &Yv[13], "YvSp2/D");
    //tree->Branch("ZvSp2", &Zv[13], "ZvSp2/D");
    //tree->Branch("XSp3",   &X[14],  "XSp3/D");
    //tree->Branch("YSp3",   &Y[14],  "YSp3/D");
    //tree->Branch("ZSp3",   &Z[14],  "ZSp3/D");
    //tree->Branch("XpSp3", &Xp[14], "XpSp3/D");
    //tree->Branch("YpSp3", &Yp[14], "YpSp3/D");
    //tree->Branch("XvSp3", &Xv[14], "XvSp3/D");
    //tree->Branch("YvSp3", &Yv[14], "YvSp3/D");
    //tree->Branch("ZvSp3", &Zv[14], "ZvSp3/D");
    
    
    if(flag==0){
      //==WC
      tree->Branch("WC1pid",  &WCpid[0],   "WC1pid/I");
      tree->Branch("WC2pid",  &WCpid[1],   "WC2pid/I");
      for(int j=0;j<2;j++){
	for(int i=0;i<12;i++){	
	  sprintf(sXWC,   "XWC%d",    100*(j+1)+i+1); 
	  sprintf(sXWCD,  "XWC%d/D",  100*(j+1)+i+1); 
	  sprintf(sYWC,   "YWC%d",    100*(j+1)+i+1); 
	  sprintf(sYWCD,  "YWC%d/D",  100*(j+1)+i+1); 
	  sprintf(sZWC,   "ZWC%d",    100*(j+1)+i+1); 
	  sprintf(sZWCD,  "ZWC%d/D",  100*(j+1)+i+1); 
	  sprintf(sWCadc, "AWC%d",  100*(j+1)+i+1); 
	  sprintf(sWCadcD,"AWC%d/D",100*(j+1)+i+1); 
	  sprintf(sWCtdc, "TWC%d",  100*(j+1)+i+1); 
	  sprintf(sWCtdcD,"TWC%d/D",100*(j+1)+i+1); 
	  tree->Branch(sXWC,    &WCx[j][i],     sXWCD);
	  tree->Branch(sYWC,    &WCy[j][i],     sYWCD);
	  tree->Branch(sZWC,    &WCz[j][i],     sZWCD);
	  tree->Branch(sWCadc,  &WCadc[j][i],   sWCadcD);
	  tree->Branch(sWCtdc,  &WCtdc[j][i],   sWCtdcD);
	}
      }
      
      //==AC
      tree->Branch("AC1pid",  &ACpid[0],   "AC1pid/I");
      tree->Branch("AC2pid",  &ACpid[1],   "AC2pid/I");
      tree->Branch("AC3pid",  &ACpid[2],   "AC3pid/I");
      for(int j=0;j<3;j++){
	for(int i=0;i<7;i++){	
	  sprintf(sXAC,   "XAC%d",    100*(j+1)+i+1); 
	  sprintf(sXACD,  "XAC%d/D",  100*(j+1)+i+1); 
	  sprintf(sYAC,   "YAC%d",    100*(j+1)+i+1); 
	  sprintf(sYACD,  "YAC%d/D",  100*(j+1)+i+1); 
	  sprintf(sACadc, "AAC%d",  100*(j+1)+i+1); 
	  sprintf(sACadcD,"AAC%d/D",100*(j+1)+i+1); 
	  sprintf(sACtdc, "TAC%d",  100*(j+1)+i+1); 
	  sprintf(sACtdcD,"TAC%d/D",100*(j+1)+i+1); 
	  tree->Branch(sXAC,    &ACx[j][i],     sXACD);
	  tree->Branch(sYAC,    &ACy[j][i],     sYACD);
	  tree->Branch(sACadc,  &ACadc[j][i],   sACadcD);
	  tree->Branch(sACtdc,  &ACtdc[j][i],   sACtdcD);
	}
      }
      
      //==TOF
      //====TOFpid
      tree->Branch("TOF1Xpid",   &TOFpid[0],  "TOF1Xpid/I");  
      tree->Branch("TOF1Ypid",   &TOFpid[1],  "TOF1Ypid/I");  
      tree->Branch("TOF2Xpid",   &TOFpid[2],  "TOF2Xpid/I");   
      //====TOFtdcsum
      tree->Branch("TTOF1X",   &TOFtdcSum[0],  "TTOF1X/D");  
      tree->Branch("TTOF1Y",   &TOFtdcSum[1],  "TTOF1Y/D");  
      tree->Branch("TTOF2X",   &TOFtdcSum[2],  "TTOF2X/D");   
      //====TOF tracklength
      tree->Branch("LTOF1X",   &TOFLength[0],  "LTOF1X/D");  
      tree->Branch("LTOF1Y",   &TOFLength[1],  "LTOF1Y/D");  
      tree->Branch("LTOF2X",   &TOFLength[2],  "LTOF2X/D");   
      for(int j=0;j<3;j++){
	for(int i=0;i<18;i++){	
	  sprintf(sXTOF,   "XTOF%d",  100*(j+1)+i+1); 
	  sprintf(sXTOFD,  "XTOF%d/D",100*(j+1)+i+1); 
	  sprintf(sYTOF,   "YTOF%d",  100*(j+1)+i+1); 
	  sprintf(sYTOFD,  "YTOF%d/D",100*(j+1)+i+1); 
	  sprintf(sTOFa,   "ATOF%d",  100*(j+1)+i+1); 
	  sprintf(sTOFaD,  "ATOF%d/D",100*(j+1)+i+1); 
	  sprintf(sTOFt,   "TTOF%d",  100*(j+1)+i+1); 
	  sprintf(sTOFtD,  "TTOF%d/D",100*(j+1)+i+1); 
	  tree->Branch(sXTOF,  &TOFx[j][i],     sXTOFD);
	  tree->Branch(sYTOF,  &TOFy[j][i],     sYTOFD);
	  tree->Branch(sTOFa,  &TOFadc[j][i],   sTOFaD);
	  tree->Branch(sTOFt,  &TOFtdc[j][i],   sTOFtD);
	}
      }
    }//flag==0
  }//flag==0 || flag==1
  
  if (KDCFlag!=0){
    //===Drift Chamber===//
    for (int i=0;i<6;i++){
      sprintf(sPKDC,"PKDC%d",i+1); 
      sprintf(sPKDCD,"PKDC%d/D",i+1); 
      sprintf(sXKDC,"XKDC%d",i+1); 
      sprintf(sXKDCD,"XKDC%d/D",i+1); 
      sprintf(sYKDC,"YKDC%d",i+1); 
      sprintf(sYKDCD,"YKDC%d/D",i+1); 
      sprintf(sZKDC,"ZKDC%d",i+1); 
      sprintf(sZKDCD,"ZKDC%d/D",i+1); 
      sprintf(sXpKDC,"XpKDC%d",i+1); 
      sprintf(sXpKDCD,"XpKDC%d/D",i+1); 
      sprintf(sYpKDC,"YpKDC%d",i+1); 
      sprintf(sYpKDCD,"YpKDC%d/D",i+1); 
      tree->Branch(sPKDC,    &KDC1P[i],   sPKDCD);
      tree->Branch(sXKDC,    &KDC1X[i],   sXKDCD);
      tree->Branch(sYKDC,    &KDC1Y[i],   sYKDCD);
      tree->Branch(sZKDC,    &KDC1Z[i],   sZKDCD);
      tree->Branch(sXpKDC,   &KDC1Xp[i],   sXpKDCD);
      tree->Branch(sYpKDC,   &KDC1Yp[i],   sYpKDCD);
    }
    for (int i=0;i<6;i++){
      sprintf(sPKDC,"PKDC%d",i+7); 
      sprintf(sPKDCD,"PKDC%d/D",i+7); 
      sprintf(sXKDC,"XKDC%d",i+7); 
      sprintf(sXKDCD,"XKDC%d/D",i+7); 
      sprintf(sYKDC,"YKDC%d",i+7); 
      sprintf(sYKDCD,"YKDC%d/D",i+7); 
      sprintf(sZKDC,"ZKDC%d",i+7); 
      sprintf(sZKDCD,"ZKDC%d/D",i+7); 
      sprintf(sXpKDC,"XpKDC%d",i+7); 
      sprintf(sXpKDCD,"XpKDC%d/D",i+7); 
      sprintf(sYpKDC,"YpKDC%d",i+7); 
      sprintf(sYpKDCD,"YpKDC%d/D",i+7); 
      tree->Branch(sPKDC,    &KDC2P[i],   sPKDCD);
      tree->Branch(sXKDC,    &KDC2X[i],   sXKDCD);
      tree->Branch(sYKDC,    &KDC2Y[i],   sYKDCD);
      tree->Branch(sZKDC,    &KDC2Z[i],   sZKDCD);
      tree->Branch(sXpKDC,   &KDC2Xp[i],   sXpKDCD);
      tree->Branch(sYpKDC,   &KDC2Yp[i],   sYpKDCD);
    }
  }
   tree->Branch("Trig",    &Trig,  "Trig/B");
   tree->Branch("VDTrig",    &VD,  "VDTrig/B");
   tree->Branch("KDCTrig",    &KDCTrig,  "KDCTrig/B");
   tree->Branch("TOF1Trig",    &TOF1Trig,  "TOF1Trig/B");
   tree->Branch("TOF2Trig",    &TOF2Trig,  "TOF2Trig/B");
   tree->Branch("TOFTrig",    &TOFTrig,  "TOFTrig/B");
   tree->Branch("AC1Trig",    &AC1Trig,  "AC1Trig/B");
   tree->Branch("AC2Trig",    &AC2Trig,  "AC2Trig/B");
   tree->Branch("AC3Trig",    &AC3Trig,  "AC3Trig/B");
   tree->Branch("WC1Trig",    &WC1Trig,  "WC1Trig/B");
   tree->Branch("WC2Trig",    &WC2Trig,  "WC2Trig/B");

}

/////////////////////////////////////////////////////
void HKSAnalysis::SetDataFile(const char *datafile )
/////////////////////////////////////////////////////
{
  DataFile_.open( datafile );
}

/////////////////////////////////////////////////////////////////
G4double HKSAnalysis::RandGauss( G4double center, G4double dev )
/////////////////////////////////////////////////////////////////
{
  G4double rand1 = G4UniformRand();
  G4double rand2 = G4UniformRand();

  G4double a = sqrt(-2.0*log(rand1)) * cos(2.0*M_PI*rand2);

  return dev*a + center;
}
