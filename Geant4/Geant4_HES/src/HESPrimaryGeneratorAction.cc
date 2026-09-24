/*
  HESPrimaryGeneratorAction.cc

  D.Kawama
*/

#include "HESPrimaryGeneratorAction.hh"
#include "HESAnalysis.hh"

#include "G4ParticleGun.hh"
#include "G4ParticleTypes.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4ParticleMomentum.hh"
#include "G4Event.hh"
#include "Randomize.hh"
#include "G4LorentzVector.hh"

#include <cmath>
#include <iostream>
#include <cstdio>

#include "HESParamMan.hh"

  G4ParticleDefinition* particle;
//Particle ID
/////////////////////////////////////////////////////////////
HESPrimaryGeneratorAction::
HESPrimaryGeneratorAction( HESDetectorConstruction* , 
			   HESAnalysis* analysisManager )
  :G4VUserPrimaryGeneratorAction(), anaMan(analysisManager)
/////////////////////////////////////////////////////////////
{
  HESParamMan *paramMan = HESParamMan::GetParamMan();
  int pID = paramMan->GetParticleID();
  G4ParticleTable* particleTable 
                  = G4ParticleTable::GetParticleTable();
  if ( pID==1 ){
    particle = particleTable->FindParticle("e-");
  }
  else if (pID==2){
    particle = particleTable->FindParticle("kaon+");
  }
  else{
    particle = particleTable->FindParticle("geantino");
  }
  dataFile=fopen( paramMan->GetInputDataName(), "r");
  //CLHEP::HepRandom::setTheEngine(new CLHEP::RanluxEngine);
  //CLHEP::HepRandom::setTheSeed(time(NULL),4);

}

/////////////////////////////////////////////////////////
HESPrimaryGeneratorAction::~HESPrimaryGeneratorAction()
////////////////////////////////////////////////////////
{
  if ( dataFile ) fclose( dataFile );
}

//Generator event action
//////////////////////////////////////////////////////////////////////
void HESPrimaryGeneratorAction::GeneratePrimaries( G4Event* anEvent )
////////////////////////////////////////////////////////////////////
{
  HESParamMan *paramMan = HESParamMan::GetParamMan();
  int generateID = paramMan->GetGenerationID();
  if ( generateID == 0 ) {
    return GenParUni( anEvent );
  }
  else {
    return GenFromFile( anEvent );
  }
}

///////////////////////////////////////////////////////////////
void HESPrimaryGeneratorAction::GenerateBeam( G4Event* )
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
void HESPrimaryGeneratorAction::GenParUni( G4Event* anEvent )
/////////////////////////////////////////////////////////////////////
{
  HESParamMan *paramMan = HESParamMan::GetParamMan();

  G4double prepx,prepy,prepz; 
  G4double px,py,pz; 
  G4double E_Theta = paramMan->GetRotAngle()*degree;
  
  G4double CentMom = paramMan->GetCentMom();
  G4double AcptMom = paramMan->GetAcptMom();
  G4double CentTheta = paramMan->GetCentTheta();
  G4double AcptTheta = paramMan->GetAcptTheta();
  G4double CentPhi = paramMan->GetCentPhi();
  G4double AcptPhi = paramMan->GetAcptPhi();
  G4double xR = paramMan->GetRasterX();
  G4double yR = paramMan->GetRasterY();
  G4double zR = paramMan->GetRasterZ();
  G4double Mom = (CentMom + (G4UniformRand()-0.5)*2.0*AcptMom);
  G4double min = cos(CentTheta+AcptTheta);
  G4double max = cos(CentTheta-AcptTheta);
  G4double da = (max-min)/2. ;
  G4double a = min + da ;
  
  /*G4double momFlag = G4UniformRand();
  G4double Mom=0;
  if(momFlag<=0.3){
    Mom = CentMom*0.85;
  }
  else if(0.3<momFlag && momFlag<0.7){
    Mom = CentMom;
  }
  else {
    Mom = CentMom*1.15;
  }*/
  
  G4double theta=acos( a + ( 2.*da*(G4UniformRand()-0.5) ) );
  G4double phi=CentPhi+( 2*AcptPhi*( G4UniformRand()-0.5 ) );
  G4double xp=sin(theta)*cos(phi)/cos(theta);
  G4double yp=sin(theta)*sin(phi)/cos(theta);
  
  prepz = Mom/sqrt(1+xp*xp+yp*yp);
  prepx = prepz*xp;
  prepy = prepz*yp;
  px = prepx*cos(E_Theta)-prepz*sin(E_Theta);
  py = prepy;
  pz = prepx*sin(E_Theta)+prepz*cos(E_Theta);
  //G4double prex = xR*(G4UniformRand()-0.5)*2.;
  //G4double prez = zR*(G4UniformRand()-0.5)*2.;
  //G4double prez = -5.;
  //G4double prey = yR*(G4UniformRand()-0.5)*2.;
  G4double prex = xR*(G4UniformRand()-0.5)*2./2.;//toshi
  G4double prey = yR*(G4UniformRand()-0.5)*2./2.;//toshi
  G4double prez = zR*(G4UniformRand()-0.5)*2./2.;//toshi
  G4double x = prex*cos(E_Theta)-prez*sin(E_Theta);
  G4double y = prey;
  G4double z = prex*sin(E_Theta)+prez*cos(E_Theta);
  G4ThreeVector gPos(x*cm, y*cm, z*cm);
  //G4ParticleMomentum gMom(px*GeV, py*GeV, pz*GeV);
  G4ThreeVector gMom(px*GeV, py*GeV, pz*GeV);
  
  SetMom(gMom,gPos,anEvent,0);

}

////////////////////////////////////////////////////////////////////
void HESPrimaryGeneratorAction::GenFromFile( G4Event *anEvent )
////////////////////////////////////////////////////////////////////
{
  HESParamMan *paramMan = HESParamMan::GetParamMan();
  G4double E_Theta = paramMan->GetRotAngle()*degree;
  G4double CentMom = paramMan->GetCentMom();
  G4double xR = paramMan->GetRasterX();
  G4double yR = paramMan->GetRasterY();
  G4double zR = paramMan->GetRasterZ();
  char str[MAXCHAR];
  fgets( str, MAXCHAR, dataFile );
  
  G4double px, py, pz;
  G4double Mom=0.;
  G4double prepx; 
  G4double prepy; 
  G4double prepz; 
  G4double x0, y0, z0;
  G4double t, dp,pe, prob;
  G4double xp, yp;
  G4double xp0, yp0;
  G4double xpk,ypk,pk;
  G4double pbeam0,pbeam1;
  G4int evID;
  
  if (sscanf( str, "%d %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
	      &evID,  &x0, &xp0, &y0, &yp0, &pe, &z0, &xpk, &ypk, &pk, &pbeam1, &pbeam0)==12){
    //G4cout <<  "  ; ; ;; ;" << G4endl;
    //G4cout << x0 << " " << y0 << " " << z0 << " "  << G4endl;
    Mom = pe;
    paramMan->SetpBeam(pbeam0,pbeam1);
  }
 else if (sscanf( str, "%d %lf %lf %lf %lf %lf %lf %lf %lf %lf",
	      &evID,  &x0, &xp0, &y0, &yp0, &pe, &z0, &xpk, &ypk, &pk)==10){
   //G4cout << " 10" << G4endl;
    Mom = pe;
  }
  else if(sscanf( str, "%lf %lf %lf %lf %lf %lf %lf %lf",
		  &x0, &xp0, &y0, &yp0, &z0, &t, &dp, &prob)==8){
    Mom = CentMom*(1.+(dp/100.));
  }
  else{std::cout << "*****I can't read seed file ...*****"<< std::endl;}
 //G4cout << pbeam1 << " " << pbeam0 << G4endl;
  xp = xp0/1000.;
  yp = yp0/1000.;
  prepz = (Mom/sqrt(1+xp*xp+yp*yp));
  prepx = prepz*xp;
  prepy = prepz*yp;
  px = prepx*cos(E_Theta)-prepz*sin(E_Theta);
  py = prepy;
  pz = prepx*sin(E_Theta)+prepz*cos(E_Theta);
  // ~~~~~~~~ Original ~~~~~~~~~~~~~~~~
  //G4double x = xR*(x0*cos(E_Theta)-z0*sin(E_Theta));
  //G4double y = yR*y0;
  //G4double z = zR*(x0*sin(E_Theta)+z0*cos(E_Theta));
  
  G4ThreeVector gMom( px*GeV, py*GeV, pz*GeV);
  //G4ParticleMomentum gMom( px*GeV, py*GeV, pz*GeV);
  //G4ThreeVector gPos( x*cm, y*cm, z*cm);
  G4ThreeVector gPos( x0*cm, y0*cm, z0*cm);
  //G4ThreeVector gPos( 0.*cm, 0.*cm, 0.*cm);

  SetMom(gMom,gPos,anEvent,evID);
}

//////////////////////////////////////////
void HESPrimaryGeneratorAction::SetMom
( G4ParticleMomentum gMom, G4ThreeVector gPos, G4Event* anEvent,G4int evID)
//////////////////////////////////////////
{
  //G4cout << gMom.x()/GeV << gMom.y()/GeV << gMom.z()/GeV <<G4endl;

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
