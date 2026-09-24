/*
  ChamberExtension.hh

  H.Nomura
  2003/Dec/4
*/


#ifndef ChamberExtension_h
#define ChamberExtension_h 1

#include "BeamLineElement.hh"

#include <vector>

class G4Material;
class G4VPhysicalVolume;
class G4VSolid;
class G4LogicalVolume;
class G4UserLimits;

class ChamberExtension : public BeamLineElement
{
public:
  ChamberExtension( const G4String &name, 
		    const G4ThreeVector &gPos,
		    const G4RotationMatrix *rotMtx);
  ~ChamberExtension();

  // Overloaded Functions
  bool ExistMagneticField( void ) const;
  bool ExistElectricField( void ) const
  { return false; }
  bool IsInside( const G4ThreeVector &gPos ) const;

  //  G4ThreeVector GetMagneticField( const G4ThreeVector &gPos ) const;

  void SetMaterials( G4Material *mYoke ,G4Material *mGap , G4Material *mWindow )
  { mYoke_ = mYoke; mGap_ = mGap; mWind_ = mWindow; }
  G4VPhysicalVolume* Place( G4VPhysicalVolume* pMother );

  void SetUserLimits( G4UserLimits *limitYoke,
		      G4UserLimits *limitGap = 0 );

private:
  G4Material *mYoke_, *mGap_, *mWind_;
  G4VSolid *solidField;
  G4LogicalVolume *gapLV_, *yokeLV_;
  
};

#endif
