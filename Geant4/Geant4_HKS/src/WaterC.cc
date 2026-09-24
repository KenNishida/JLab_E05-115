/*
  WaterC.cc
  for Water Cherenkov Counter
  H.Nomura 2003/Dec/31
*/

#include "WaterC.hh"

#include "G4Material.hh"
#include "G4Box.hh"
#include "G4SubtractionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

WaterC::WaterC( const G4String &name,
		const G4ThreeVector &gPos,
		const G4RotationMatrix *rotMtx,
		int id )
  : BeamLineElement(name,gPos,rotMtx),
    mBox_(0), mGas_(0), mRad_(0), DetectorLV_(0), vSolid_(0), Id_(id)
{
}

WaterC::~WaterC()
{
}

bool WaterC::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  if( vSolid_-> Inside(lPos) != kOutside ) return true;
  else return false;
}

G4VPhysicalVolume *WaterC::Place( G4VPhysicalVolume *pMother )
{
  G4String nmWall = elemName_+" Wall";
  G4String nmGas = elemName_+" Gas";

  G4ThreeVector dxyz_WC_Wall( 187.2*cm, 36.0*cm, 8.6*cm );
  G4ThreeVector dxyz_WC( 15.*cm, 35.*cm, 8.*cm );

  // definition of solid
  G4Box *WallSolid = new G4Box( nmWall, dxyz_WC_Wall.x()/2,
				dxyz_WC_Wall.y()/2, dxyz_WC_Wall.z()/2 );
  G4Box *wcSolid = new G4Box( elemName_, 
			      dxyz_WC.x()/2, dxyz_WC.y()/2, dxyz_WC.z()/2 );

  // definition of Logical Volume
  G4LogicalVolume *WallLV = new G4LogicalVolume( WallSolid, mBox_, nmWall );
  G4LogicalVolume *wcLV = new G4LogicalVolume( wcSolid, mRad_, elemName_ );

  // establish Physical Volume
  G4VPhysicalVolume *Wall =
    new G4PVPlacement( &rotMtx_, gPos_, nmWall, WallLV,
		       pMother, false, Id_ );
  for( int i=0; i<12; ++i ){
    G4double x = ( 85.8 -15.6*i )*cm;
    new G4PVPlacement( 0, G4ThreeVector(x,0.,0.),
				      elemName_, wcLV, Wall, false, i );
  }

  G4VisAttributes *wcVisAtt =  new G4VisAttributes();
    //new G4VisAttributes( G4Colour::G4Colour(0.,0.,1.) );
  wcVisAtt->SetColour(0.0 , 0.0 , 1.0);
  wcLV-> SetVisAttributes( wcVisAtt );

  DetectorLV_ = wcLV;
  vSolid_ = WallSolid;
  
  //delete wcVisAtt;

  return Wall;
}
