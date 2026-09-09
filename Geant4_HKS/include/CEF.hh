/*
  Chamber extension flange
  2 Nov 2010
  T.Gogami
*/

#ifndef CEF_h
#define CEF_h 1

#include "MaterialList.hh"

#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"
class G4Material;
class G4VSolid;
class G4LogicalVolume;
class G4VPhysicalVolume;


class CEF
{
public:
  //Constructor
  CEF( G4VPhysicalVolume* , MaterialList* );
  ~CEF();
  
private:
  G4VPhysicalVolume *mother; //World
  MaterialList* mList;       //Material List
public:
  void Place(G4RotationMatrix* , G4ThreeVector 
	     , G4RotationMatrix* , G4ThreeVector );
};

#endif
