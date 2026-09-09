/*
  HKSPrimaryGeneratorAction.cc

  M.Kawai
*/

#include "HKSPrimaryGeneratorAction.hh"
#include "HKSAnalysis.hh"

#include "G4ParticleGun.hh"
#include "G4ParticleTypes.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4Event.hh"
#include "Randomize.hh"
#include "G4LorentzVector.hh"
#include <CLHEP/Random/RandFlat.h>

#include <cmath>
#include <iostream>
#include <cstdio>

#include "HKSParamMan.hh"

G4ParticleDefinition* particle;
//Particle ID
/////////////////////////////////////////////////////////////
HKSPrimaryGeneratorAction::
HKSPrimaryGeneratorAction( HKSDetectorConstruction* , 
			   HKSAnalysis* analysisManager )
  :G4VUserPrimaryGeneratorAction(), anaMan(analysisManager)
/////////////////////////////////////////////////////////////
{
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  int pID = paramMan->GetParticleID();
  G4ParticleTable* particleTable 
                  = G4ParticleTable::GetParticleTable();
  if ( pID==1 ){
    particle = particleTable->FindParticle("proton");
  }
  else if (pID==2){
    particle = particleTable->FindParticle("kaon+");
  }
  else if (pID==3){
    particle = particleTable->FindParticle("pi+");
  }
  else if (pID==4){
    particle = particleTable->FindParticle("e+");
  }
  else if (pID==5){
    particle = particleTable->FindParticle("gamma");
  }
  else{
    particle = particleTable->FindParticle("e-");
  }
  	dataFile=fopen( paramMan->GetInputDataName(), "r");

  //CLHEP::HepRandom::setTheEngine(new CLHEP::RanluxEngine);
  //CLHEP::HepRandom::setTheSeed(time(NULL),4);

}

/////////////////////////////////////////////////////////
HKSPrimaryGeneratorAction::~HKSPrimaryGeneratorAction()
////////////////////////////////////////////////////////
{
  if ( dataFile ) fclose( dataFile );
}

//Generator event action
//////////////////////////////////////////////////////////////////////
void HKSPrimaryGeneratorAction::GeneratePrimaries( G4Event* anEvent )
////////////////////////////////////////////////////////////////////
{
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  int generateID = paramMan->GetGenerationID();
  if ( generateID == 0 ) {
    return GenParUni( anEvent );
  }
  else {
    return GenFromFile( anEvent );
  }
}

///////////////////////////////////////////////////////////////
void HKSPrimaryGeneratorAction::GenerateBeam( G4Event* )
///////////////////////////////////////////////////////////////
{
/*  G4ThreeVector prodPoint(0.*cm, 0.*cm, -1.*cm);
  G4ThreeVector BeamMom(0.*GeV, 0.*GeV, 2.5*GeV);

  gunParticle-> SetParticleEnergy(0.0); // reset
  gunParticle-> SetParticleMomentum(BeamMom);
  gunParticle-> SetParticlePosition(prodPoint);
  gunParticle-> GeneratePrimaryVertex(anEvent);

  if( anaMan ){
    anaMan-> PrimaryGeneration(prodPoint, BeamMom);
  }*/
}

/////////////////////////////////////////////////////////////////////
void HKSPrimaryGeneratorAction::GenParUni( G4Event* anEvent )
/////////////////////////////////////////////////////////////////////
{
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();

  G4double px,py,pz; 
  //G4double CentMom = 0.1;//GeV/c
  G4double CentMom = paramMan->GetCentMom();
  G4double AcptMom = paramMan->GetAcptMom();
  G4double CentTheta = paramMan->GetCentTheta();
  G4double AcptTheta = paramMan->GetAcptTheta();
  G4double CentPhi = paramMan->GetCentPhi();
  G4double AcptPhi = paramMan->GetAcptPhi();
  G4double ThetaLimit = paramMan->GetThetaLimit();
  G4double xR = paramMan->GetRasterX();
  G4double yR = paramMan->GetRasterY();
  G4double zR = paramMan->GetRasterZ();
  G4double Mom = (CentMom + (G4UniformRand()-0.5)*2.0*AcptMom);
  G4double xp = 0.;
  G4double yp = 0.;
  G4int RanSign=rand()%2;
  G4double phi=0;
  G4double min = cos(CentTheta+AcptTheta);
  G4double max = cos(CentTheta-AcptTheta);
  G4double da = (max-min)/2. ;
  G4double a = min + da ;

  G4double theta=acos( a + ( 2.*da*(G4UniformRand()-0.5) ) );
  if(RanSign==1)phi=-1.*(CentPhi+( 2.0*AcptPhi*( G4UniformRand()-0.5 ) ));
  else if(RanSign==0)phi=1.*(CentPhi+( 2.0*AcptPhi*( G4UniformRand()-0.5 ) ));
  xp=sin(theta)*cos(phi)/cos(theta);
  yp=sin(theta)*sin(phi)/cos(theta);
  pz = Mom/sqrt(1+xp*xp+yp*yp);
  px = pz*xp;
  py = pz*yp;
  //G4double x = xR*(G4UniformRand()-0.5)*2.*cm;
  //G4double y = yR*(G4UniformRand()-0.5)*2.*cm;
  //G4double z = (zR*(G4UniformRand()-0.5)*2.)*cm;
  G4double x = xR*(G4UniformRand()-0.5)*2./2.0*cm;//toshi
  G4double y = yR*(G4UniformRand()-0.5)*2./2.0*cm;//toshi
  G4double z = (zR*(G4UniformRand()-0.5)*2./2.0)*cm;//toshi
  G4ThreeVector gPos(x, y, z);
  G4ThreeVector gMom(px*GeV, py*GeV, pz*GeV);
  //G4cout << gMom.x() << G4endl;
  SetMom(gMom,gPos,anEvent,0);
}

////////////////////////////////////////////////////////////////////
void HKSPrimaryGeneratorAction::GenFromFile( G4Event *anEvent )
////////////////////////////////////////////////////////////////////
{
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  char str[MAXCHAR];
  fgets( str, MAXCHAR, dataFile );
  
  G4double px, py, pz, Mom;
  G4double x0, y0, z0;
  G4double pe;
  G4double xp, yp;
  G4double xp0, yp0;
  G4double xpk,ypk,pk;
  G4double xR = paramMan->GetRasterX();
  G4double yR = paramMan->GetRasterY();
  G4double zR = paramMan->GetRasterZ();
  G4int evID;
  double temp;

//  sscanf( str, "%d %lf %lf %lf %lf %lf %lf %lf %lf %lf",
//          &evID,  &x0, &xp0, &y0, &yp0, &pe, &z0, &xpk, &ypk, &pk);
  sscanf( str, "%d %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
          &evID,  &x0, &xp0, &y0, &yp0, &pe, &z0, &xpk, &ypk, &pk, &temp, &temp);
  Mom = pk;
  //G4cout << x0 << G4endl;

  xp = xpk/1000.;
  yp = ypk/1000.;
  pz = (Mom/sqrt(1+xp*xp+yp*yp));
  px = pz*xp;
  py = pz*yp;
  G4ThreeVector gMom( px*GeV, py*GeV,pz*GeV);  
  //G4ThreeVector gPos( xR*x0*cm, yR*y0*cm, zR*z0*cm);
  G4ThreeVector gPos( x0*cm, y0*cm, z0*cm);
  //G4ThreeVector gPos( 0.*cm, 0.*cm, 0.*cm);

  SetMom(gMom,gPos,anEvent,evID);
}

void HKSPrimaryGeneratorAction::SetMom
(G4ThreeVector gMom, G4ThreeVector gPos, G4Event* anEvent, G4int evID)
{
  G4int n_particle = 1;
  gunParticle = new G4ParticleGun(n_particle);
  gunParticle-> SetParticleEnergy(0.0*GeV); //reset
  gunParticle-> SetParticleDefinition(particle); 
  gunParticle-> SetParticleMomentum(gMom);
  gunParticle-> SetParticlePosition(gPos);
  gunParticle-> GeneratePrimaryVertex(anEvent);

  if( anaMan ){
    anaMan->PrimaryGeneration( gPos, gMom, evID );
    //G4cout << gPos.x() << G4endl;
  }
  delete gunParticle;

}
