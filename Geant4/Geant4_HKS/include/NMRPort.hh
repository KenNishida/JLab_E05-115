/*
  NMR Port of HKS
  2 Nov 2010
  T.Gogami
*/

#ifndef NMRPort_h
#define NMRPort_h 1

#include "MaterialList.hh"

#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"
class G4Material;
class G4VSolid;
class G4LogicalVolume;
class G4VPhysicalVolume;


class NMRPort
{
public:
  //Constructor
  NMRPort( G4VPhysicalVolume* , MaterialList* );
  ~NMRPort();
  
private:
  G4VPhysicalVolume *mother; //World
  MaterialList* mList;       //Material List
public:
  void Place(G4RotationMatrix* 
	     , G4ThreeVector 
	     , G4ThreeVector 
	     , G4ThreeVector 
	     , G4ThreeVector );
	     
};

#endif
