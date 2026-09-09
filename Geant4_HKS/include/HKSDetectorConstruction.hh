/*
  HKSDetectorConstruction.hh

  2003/Oct/29
  H.Nomura
*/

#ifndef HKSDetectorConstruction_h

#define HKSDetectorConstruction_h 1

#include "globals.hh"
#include "G4VUserDetectorConstruction.hh"
#include "G4ThreeVector.hh"

class MaterialList;
class HKSField;

class HKSDetectorConstruction : public G4VUserDetectorConstruction
{
public:
  HKSDetectorConstruction();
  ~HKSDetectorConstruction();

  G4VPhysicalVolume* Construct();

private:
  MaterialList *mList_;
  MaterialList *DefineMaterials( void );

  HKSField *MakeDetectorField( void );
  HKSField *EMField_;

  G4VPhysicalVolume* ConstructPayload();

};

#endif

