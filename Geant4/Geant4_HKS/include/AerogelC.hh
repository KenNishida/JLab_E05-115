/*
  AerogelC.hh
  header file for Aerogel Cherenkov Counter

  H.Nomura 2003/Dec/31
*/

#ifndef AerogelC_h
#define AerogelC_h 1

#include "BeamLineElement.hh"

#include <vector>

class G4Material;
class G4VSolid;
class G4LogicalVolume;
class G4VPhysicalVolume;

class AerogelC : public BeamLineElement
{
public:
  AerogelC( const G4String &name, const G4ThreeVector &gPos,
	    const G4RotationMatrix *rotMtx, int id );

  ~AerogelC();

  bool ExistElectricField( void ) const { return false; }
  bool ExistMagneticField( void ) const { return false; }

  bool IsInside( const G4ThreeVector &gPos ) const;

  void SetMaterials( G4Material *mBox, G4Material *mGas, G4Material *mAC)
  { mBox_=mBox; mGas_=mGas; mRad_=mAC; }

  G4LogicalVolume *GetDetectorLV( void ) { return DetectorLV_; }

  G4VPhysicalVolume *Place( G4VPhysicalVolume *pMother );

private:
  G4Material *mBox_, *mGas_, *mRad_;
  G4LogicalVolume *DetectorLV_;
  G4VSolid *vSolid_;
  int Id_;
};

#endif
