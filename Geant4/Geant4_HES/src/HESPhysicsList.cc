/*
  HESPhysicsList.cc
  2009/12 D.Kawama
  Ref. N03 
*/

#include "globals.hh"
#include "HESPhysicsList.hh"
#include "HESTransportation.hh"
#include "HESParamMan.hh"

#include "G4ParticleDefinition.hh"
#include "G4ParticleWithCuts.hh"
#include "G4ProcessManager.hh"
#include "G4ProcessVector.hh"
#include "G4ParticleTypes.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4PhaseSpaceDecayChannel.hh"
#include "G4KL3DecayChannel.hh"
#include "G4DecayTable.hh"
#include "G4Material.hh"
#include "G4OpticalPhoton.hh"
#include "G4Ions.hh"
#include "G4ios.hh"
#include <iomanip>

#include "G4BaryonConstructor.hh"
#include "G4ShortLivedConstructor.hh"


HESPhysicsList::HESPhysicsList()
  : G4VUserPhysicsList()
{
  defaultCutValue = 0.1*cm;
}

HESPhysicsList::~HESPhysicsList()
{
}

void HESPhysicsList::ConstructParticle()
{
  ConstructBosons();
  ConstructLeptons();
  ConstructMesons();
  ConstructBaryons();
  //ConstructHeavyIon();
}

void HESPhysicsList::ConstructBosons()
{
  // pseudo-particles
  G4Geantino::GeantinoDefinition();
  G4ChargedGeantino::ChargedGeantinoDefinition();
  // gamma
  G4Gamma::GammaDefinition();
  // optical photon
  G4OpticalPhoton::OpticalPhotonDefinition();
}

void HESPhysicsList::ConstructLeptons()
{
  // leptons
  //  e+/-
  G4Electron::ElectronDefinition();
  G4Positron::PositronDefinition();
  // mu+/-
  G4MuonPlus::MuonPlusDefinition();
  G4MuonMinus::MuonMinusDefinition();
  // nu_e
  G4NeutrinoE::NeutrinoEDefinition();
  G4AntiNeutrinoE::AntiNeutrinoEDefinition();
  // nu_mu
  G4NeutrinoMu::NeutrinoMuDefinition();
  G4AntiNeutrinoMu::AntiNeutrinoMuDefinition();
}

void HESPhysicsList::ConstructMesons()
{
  //  mesons
  //    light mesons
  // Pion +/- 0
  G4PionPlus::PionPlusDefinition();
  G4PionMinus::PionMinusDefinition();
  G4PionZero::PionZeroDefinition();
  // Kain +/- 0
  G4KaonPlus::KaonPlusDefinition();
  G4KaonMinus::KaonMinusDefinition();
  G4KaonZero::KaonZeroDefinition();
  G4AntiKaonZero::AntiKaonZeroDefinition();
  G4KaonZeroLong::KaonZeroLongDefinition();
  G4KaonZeroShort::KaonZeroShortDefinition();
  // Eta
  G4Eta::EtaDefinition();
  G4EtaPrime::EtaPrimeDefinition();

  //For Kaon Decay
  G4DecayTable *decayTable;
  G4VDecayChannel *mode;
  G4ParticleDefinition *particle;

#if 1
  //K->Pi+ Pi- Pi-
  particle 
    = new G4ParticleDefinition(
			       "kaon1-",    0.493677*GeV, 5.315e-14*MeV,    -1.*eplus, 
			       0,              -1,             0,          
			       1,              -1,             0,             
			       "meson",         0,             0,            -321,
			       false,          0.0,           NULL,
			       false,         "kaon" );

  decayTable =  new G4DecayTable();
  mode  = new G4PhaseSpaceDecayChannel( "kaon1-", 1.0, 3, "pi+", "pi-", "pi-" );
  decayTable->Insert( mode );
  particle->SetDecayTable( decayTable );

  //K->e- Pi0 nue
  particle 
    = new G4ParticleDefinition (            
				"kaon2-",    0.493677*GeV, 5.315e-14*MeV,    -1.*eplus, 
				0,              -1,             0,          
				1,              -1,             0,             
				"meson",         0,             0,           -321,
				false,         0.0,            NULL,
				false,        "kaon" );
  
  decayTable =  new G4DecayTable();
  mode  = new G4PhaseSpaceDecayChannel("kaon2-",1.0, 3,
				       "e-","pi0", "anti_nu_e");
  decayTable->Insert(mode);
  particle->SetDecayTable(decayTable);  

  //K->Mu- Pi0 nuMu
  particle 
    = new G4ParticleDefinition (            
				"kaon3-",    0.493677*GeV, 5.315e-14*MeV,    -1.*eplus, 
				0,              -1,             0,          
				1,              -1,             0,             
				"meson",         0,             0,            -321,
				false,         0.0,            NULL,
				false,        "kaon" );
  
  decayTable =  new G4DecayTable();
  mode  = new G4PhaseSpaceDecayChannel("kaon3-",1.0, 3,
				       "mu-","pi0", "anti_nu_mu");
  decayTable->Insert(mode);
  particle->SetDecayTable(decayTable);

  //K->Pi- Pi0 Pi0
  particle 
    = new G4ParticleDefinition (            
				"kaon4-",    0.493677*GeV, 5.315e-14*MeV,    -1.*eplus, 
				0,              -1,             0,          
				1,              -1,             0,             
				"meson",         0,             0,            -321,
				false,         0.0,            NULL,
				false,        "kaon" );
  
  decayTable =  new G4DecayTable();
  mode  = new G4PhaseSpaceDecayChannel("kaon4-",1.0, 3,
				       "pi-","pi0", "pi0");
  decayTable->Insert(mode);
  particle->SetDecayTable(decayTable);
#endif

}

void HESPhysicsList::ConstructBaryons()
{
  G4BaryonConstructor pBaryonConstructor;
  pBaryonConstructor.ConstructParticle();

  G4ShortLivedConstructor pShortLivedConstructor;
  pShortLivedConstructor.ConstructParticle();
}

/*void HESPhysicsList::ConstructHeavyIon()
{
  //Ions
  // Deuteron
  G4Deuteron::DeuteronDefinition();
  // Triton
  G4Triton::TritonDefinition();
  // Alpha
  G4Alpha::AlphaDefinition();
}*/

void HESPhysicsList::ConstructProcess()
{
  
  HESParamMan *paramMan = HESParamMan::GetParamMan();
  int PhysicsFlag = paramMan->GetPhysicsFlag();
  AddTransportation();
  
  if (PhysicsFlag!=0){
  	ConstructEM();
  	ConstructDecay();
  	ConstructHadronic();
  }
}

///////Transportation
void HESPhysicsList::AddTransportation()
{
  HESTransportation* theTransportationProcess= new HESTransportation();

  // loop over all particles in G4ParticleTable
  theParticleIterator->reset();
  while( (*theParticleIterator)() ){
    G4ParticleDefinition* particle = theParticleIterator->value();
    G4ProcessManager* pmanager = particle->GetProcessManager();
    if ( !particle->IsShortLived() ) {
      // Add transportation process for all particles other than  "shortlived"
      if ( pmanager == 0) {
        // Error !! no process manager
        G4Exception("HESPhysicsList::AddTransportation : no process manager!");
      } 
      else {
        // add transportation with ordering = ( -1, "first", "first" )
        pmanager->AddProcess(theTransportationProcess);
        pmanager->SetProcessOrderingToFirst(theTransportationProcess,
                                            idxAlongStep);
        pmanager ->SetProcessOrderingToFirst(theTransportationProcess,
                                             idxPostStep);
      }
    }
    else {
      // shortlived particle case
    }
  }
}

///////Cut
void HESPhysicsList::SetCuts()
{
  // Suppress error message int case e/gamma/proton do not exist
  G4int temp = GetVerboseLevel();
  // Retrive verbose level
  SetVerboseLevel(temp);

  SetCutValue(defaultCutValue, "gamma");
  SetCutValue(defaultCutValue, "e-");
  SetCutValue(defaultCutValue, "e+");

  //SetCutsWithDefault();  
  theParticleIterator->reset();
  while( (*theParticleIterator)() ){
    G4ParticleDefinition *particle=theParticleIterator->value();
    particle->SetApplyCutsFlag( true );
    //////////////////////////////////////////////////////////////////////
    //    G4cout << particle->GetParticleName() << " ==> ApplyCutFlag = "
    //     << particle->GetApplyCutsFlag() << G4endl;
    //////////////////////////////////////////////////////////////////////
  }
}

///////EM
#include "G4ComptonScattering.hh"
#include "G4GammaConversion.hh"
#include "G4PhotoElectricEffect.hh"


#include "G4eMultipleScattering.hh"
#include "G4eIonisation.hh"
#include "G4eBremsstrahlung.hh"
#include "G4eplusAnnihilation.hh"

#include "G4MuMultipleScattering.hh"
#include "G4MuIonisation.hh"
#include "G4MuBremsstrahlung.hh"
#include "G4MuPairProduction.hh"

#include "G4hMultipleScattering.hh"
#include "G4hIonisation.hh"
#include "G4hBremsstrahlung.hh"
#include "G4hPairProduction.hh"

#include "G4ionIonisation.hh"

#include "G4UserSpecialCuts.hh"

void HESPhysicsList::ConstructEM()
{
  theParticleIterator->reset();
  while( (*theParticleIterator)() ){
    G4ParticleDefinition *particle = theParticleIterator->value();
    G4ProcessManager *pManager = particle->GetProcessManager();
    G4String particleName = particle->GetParticleName();

    if( particleName == "gamma" ){
      pManager->AddProcess( new G4UserSpecialCuts(), -1, -1, 1 );    
      pManager->AddDiscreteProcess( new G4PhotoElectricEffect );
      pManager->AddDiscreteProcess( new G4ComptonScattering );
      pManager->AddDiscreteProcess( new G4GammaConversion );
    }
    else if( particleName == "e-" ){
      pManager->AddProcess( new G4UserSpecialCuts,    -1, -1, 1 );
      pManager->AddProcess( new G4eMultipleScattering, -1,  1, 2 );
      pManager->AddProcess( new G4eIonisation,        -1,  2, 3 );
      pManager->AddProcess( new G4eBremsstrahlung,    -1, -1, 4 );
    }
    else if( particleName == "e+" ){
      pManager->AddProcess( new G4UserSpecialCuts,    -1, -1, 1 );
      pManager->AddProcess( new G4eMultipleScattering, -1,  1, 2 );
      pManager->AddProcess( new G4eIonisation,        -1,  2, 3 );
      pManager->AddProcess( new G4eBremsstrahlung,    -1,  3, 4 );
      pManager->AddProcess( new G4eplusAnnihilation,   0, -1, 5 );
    }
    else if( particleName == "mu+" || particleName == "mu-" ){
      pManager->AddProcess( new G4MuMultipleScattering, -1,  1, 1 );
      pManager->AddProcess( new G4MuIonisation,       -1,  2, 2 ); 
      pManager->AddProcess( new G4MuBremsstrahlung,   -1,  3, 3 );
      pManager->AddProcess( new G4MuPairProduction,   -1,  4, 4 );
    }
	 else if( particleName == "proton" ||
				 particleName == "pi-" ||
				 particleName == "pi+"    ) {
	  pManager->AddProcess(new G4hMultipleScattering, -1, 1, 1);
	  pManager->AddProcess(new G4hIonisation,         -1, 2, 2);
	  pManager->AddProcess(new G4hBremsstrahlung,     -1, 3, 3);
	  pManager->AddProcess(new G4hPairProduction,     -1, 4, 4);
	 }
	 else if ( particleName == "alpha" ||
				  particleName == "He3" ){ 
     pManager->AddProcess( new G4hMultipleScattering,  -1,  1, 1 );
     pManager->AddProcess( new G4ionIonisation,        -1,  2, 2 );
	 }
	 else if ( particleName == "GenericIon" ){
     pManager->AddProcess( new G4hMultipleScattering,  -1,  1, 1 );
     pManager->AddProcess( new G4ionIonisation,        -1,  2, 2 );
	 }
    else if( (!particle->IsShortLived()) &&
             (particle->GetPDGCharge() != 0.0) &&
	     		 (particle->GetParticleName() != "chargedgeantino")){
      pManager->AddProcess( new G4hMultipleScattering,  -1,  1, 1 );
      pManager->AddProcess( new G4hIonisation,          -1,  2, 2 );
    }
    /*else if( !particle->IsShortLived() &&
             particle->GetPDGCharge()!=0.0 &&
	     !( particleName=="chargedgeantino"
		|| particleName=="antichargedgeantino")){
      pManager->AddProcess( new G4UserSpecialCuts(),      -1, -1, 1 );
      pManager->AddProcess( new G4MultipleScattering(),   -1,  1, 2 );
      pManager->AddProcess( new G4hIonisation(),          -1,  2, 3 );
    }*/
  }
}

///////Hadron
#include "G4HadronElasticProcess.hh"
#include "G4HadronInelasticProcess.hh"
#include "G4LElastic.hh"

//Pion
#include "G4PionPlusInelasticProcess.hh"
#include "G4LEPionPlusInelastic.hh"
#include "G4PionMinusInelasticProcess.hh"
#include "G4LEPionMinusInelastic.hh"

//Kaon
#include "G4KaonPlusInelasticProcess.hh"
#include "G4LEKaonPlusInelastic.hh"
#include "G4KaonMinusInelasticProcess.hh"
#include "G4LEKaonMinusInelastic.hh"
#include "G4KaonZeroSInelasticProcess.hh"
#include "G4LEKaonZeroSInelastic.hh"
#include "G4KaonZeroLInelasticProcess.hh"
#include "G4LEKaonZeroLInelastic.hh"

//Nucleon
#include "G4ProtonInelasticProcess.hh"
#include "G4LEProtonInelastic.hh"
#include "G4NeutronInelasticProcess.hh"
#include "G4LENeutronInelastic.hh"

void HESPhysicsList:: ConstructHadronic()
{
  G4HadronElasticProcess* theElasticProcess = new G4HadronElasticProcess;
  G4LElastic* theElasticModel = new G4LElastic;
  theElasticProcess->RegisterMe(theElasticModel);
  
  theParticleIterator->reset();
  while( (*theParticleIterator)() ){
    G4ParticleDefinition* particle = theParticleIterator->value();
    G4ProcessManager* pmanager = particle->GetProcessManager();
    G4String particleName = particle->GetParticleName();
    
    //Pion      
    if( particleName == "pi+" ) {
      pmanager->AddProcess( theElasticProcess );
      G4PionPlusInelasticProcess* theInelasticProcess 
	= new G4PionPlusInelasticProcess( "inelastic" );
      G4LEPionPlusInelastic* theLEInelasticModel 
	= new G4LEPionPlusInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    else if( particleName == "pi-" ) {
      pmanager->AddProcess( theElasticProcess );
      G4PionMinusInelasticProcess* theInelasticProcess 
	= new G4PionMinusInelasticProcess( "inelastic" );
      G4LEPionMinusInelastic* theLEInelasticModel 
	= new G4LEPionMinusInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    //Kaon
    else if( particleName == "kaon+" ) {
      pmanager->AddProcess( theElasticProcess );
      G4KaonPlusInelasticProcess* theInelasticProcess 
	= new G4KaonPlusInelasticProcess( "inelastic" );
      G4LEKaonPlusInelastic* theLEInelasticModel 
	= new G4LEKaonPlusInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    else if( particleName == "kaon-" ) {
      pmanager->AddProcess( theElasticProcess );
      G4KaonMinusInelasticProcess* theInelasticProcess 
	= new G4KaonMinusInelasticProcess( "inelastic" );
      G4LEKaonMinusInelastic* theLEInelasticModel 
	= new G4LEKaonMinusInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    else if( particleName == "kaon0S" ) {
      pmanager->AddProcess( theElasticProcess );
      G4KaonZeroSInelasticProcess* theInelasticProcess 
	= new G4KaonZeroSInelasticProcess( "inelastic" );
      G4LEKaonZeroSInelastic* theLEInelasticModel 
	= new G4LEKaonZeroSInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    else if( particleName == "kaon0L" ) {
      pmanager->AddProcess( theElasticProcess );
      G4KaonZeroLInelasticProcess* theInelasticProcess 
	= new G4KaonZeroLInelasticProcess( "inelastic" );
      G4LEKaonZeroLInelastic* theLEInelasticModel 
	= new G4LEKaonZeroLInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    //Nucleon
    else if( particleName == "proton" ) {
      pmanager->AddProcess( theElasticProcess );
      G4ProtonInelasticProcess* theInelasticProcess 
	= new G4ProtonInelasticProcess( "inelastic" );
      G4LEProtonInelastic* theLEInelasticModel 
	= new G4LEProtonInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
    else if( particleName == "neutron" ) {
      pmanager->AddProcess( theElasticProcess );
      G4NeutronInelasticProcess* theInelasticProcess 
	= new G4NeutronInelasticProcess( "inelastic" );
      G4LENeutronInelastic* theLEInelasticModel 
	= new G4LENeutronInelastic;
      theInelasticProcess->RegisterMe(theLEInelasticModel);
      pmanager->AddDiscreteProcess(theInelasticProcess);
    }
  }
}

///////Decay
#include "G4Decay.hh"

void HESPhysicsList::ConstructDecay()
{
  G4Decay *theDecayProcess = new G4Decay();
  theParticleIterator->reset();
  while( (*theParticleIterator)() ){
    G4ParticleDefinition *particle = theParticleIterator->value();
    G4ProcessManager *pManager = particle->GetProcessManager();
    if( theDecayProcess->IsApplicable(*particle) ){
      pManager->AddProcess( theDecayProcess );
      pManager->SetProcessOrdering( theDecayProcess, idxPostStep );
      pManager->SetProcessOrdering( theDecayProcess, idxAtRest );
    }
  }
}
