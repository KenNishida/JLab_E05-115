/*
  DetectorHut.cc

  H.Nomura
  2003/Dec/12
*/

#include "DetectorHut.hh"

#include "G4Material.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

DetectorHut::DetectorHut( const G4String &name,
			  const G4ThreeVector &gPos,
			  const G4RotationMatrix *rotMtx )
  : BeamLineElement( name, gPos, rotMtx ),
    mVol_(0), PhysVol_(0), vSolid_(0)
{
}

DetectorHut::~DetectorHut()
{
}

bool DetectorHut::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition( gPos );
  if( vSolid_-> Inside(lPos) != kOutside ) return true;
  else return false;
}


G4VPhysicalVolume *DetectorHut::Place( G4VPhysicalVolume *pMother )
{
  G4ThreeVector dxyz_Dspace( 150.*cm, 100.*cm, 200.*cm );
  G4Box *DspaceSolid = new G4Box( elemName_,
				  dxyz_Dspace.x(),
				  dxyz_Dspace.y(),
				  dxyz_Dspace.z() );
  G4LogicalVolume *DspaceLV = 
    new G4LogicalVolume( DspaceSolid, mVol_, elemName_ );
  G4VPhysicalVolume *Dspace = 
    new G4PVPlacement( &rotMtx_, gPos_, elemName_,
		       DspaceLV, pMother, false, 0 );

  vSolid_ = DspaceSolid;
  PhysVol_ = Dspace;

  DspaceLV->SetVisAttributes( G4VisAttributes::Invisible );
  return Dspace;
}
