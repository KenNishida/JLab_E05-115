/*
  HKSGeneralPhysics.hh

  2003/Nov/3
  H.Nomura
*/

#ifndef HKSGeneralPhysics_h
#define HKSGeneralPhysics_h 1

#include "globals.hh"
#include "G4ios.hh"

#include "G4VPhysicsConstructor.hh"


#include "G4Decay.hh"

class HKSGeneralPhysics : public G4VPhysicsConstructor
{
  public: 
    HKSGeneralPhysics(const G4String& name = "general");
    virtual ~HKSGeneralPhysics();

  public: 
    // This method will be invoked in the Construct() method. 
    // each particle type will be instantiated
    virtual void ConstructParticle();
 
    // This method will be invoked in the Construct() method.
    // each physics process will be instantiated and
    // registered to the process manager of each particle type 
    virtual void ConstructProcess();

  protected:
    G4Decay fDecayProcess;
};

#endif

