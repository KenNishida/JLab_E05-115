/*
  HTOF1X.hh

  H.Nomura
  2003/Dec/12
*/

#ifndef HTOF1X_h
#define HTOF1X_h 1

#include "BeamLineElement.hh"

#include <vector>

class G4Material;
class G4LogicalVolume;
class G4VPhysicalVolume;
class G4VSolid;

class HTOF1X : public BeamLineElement
{
public:
  HTOF1X( const G4String &name, const G4ThreeVector &gPos,
	  const G4RotationMatrix *rotMtx , int id );

  ~HTOF1X();

  bool ExistElectricField( void ) const { return false; }
  bool ExistMagneticField( void ) const { return false; }

  bool IsInside( const G4ThreeVector &gPos ) const;

  void SetMaterial( G4Material *mVolume )
  { mVol_ = mVolume; }

  G4VPhysicalVolume *Place( G4VPhysicalVolume *pMother );

  G4LogicalVolume *GetDetectorLV( void ) { return DetectorLV_; }
 
private:
  G4Material *mVol_;
  G4LogicalVolume *DetectorLV_;
  G4VSolid *vSolid;
  int Id_;

};

#endif
