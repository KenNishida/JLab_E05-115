/*
  AerogelC.cc
  for Aerogel Cherenkov Counter
  H.Nomura 2003/Dec/31
*/

#include "AerogelC.hh"

#include "G4Material.hh"
#include "G4Box.hh"
#include "G4SubtractionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

AerogelC::AerogelC( const G4String &name,
		    const G4ThreeVector &gPos,
		    const G4RotationMatrix *rotMtx,
		    int id )
  : BeamLineElement(name,gPos,rotMtx),
    mBox_(0), mGas_(0), mRad_(0), DetectorLV_(0), vSolid_(0)
{
  Id_ = id;
}

AerogelC::~AerogelC()
{
}

bool AerogelC::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  if( vSolid_-> Inside(lPos) != kOutside ) return true;
  else return false;
}

G4VPhysicalVolume *AerogelC::Place( G4VPhysicalVolume *pMother )
{
  G4String nmWall = elemName_+" Wall";
  G4String nmGas = elemName_+" Gas";
  G4String nmBox = elemName_+" Box";

  G4ThreeVector dxyz_AC_Wall( 141.8*cm, 40.2*cm, 30.2*cm );
  //  G4ThreeVector dxyz_AC_Box( 20.2*cm, 40.2*cm, 30.2*cm );
  //  G4ThreeVector dxyz_AC_Gas( 20.2*cm, 40.*cm, 30.*cm );
  G4ThreeVector dxyz_AC_Gas( 141.4*cm, 40.*cm, 30.*cm );
  G4ThreeVector dxyz_AC( 20.*cm, 40.*cm, 5.*cm );

  // definition of solid
  G4Box *WallSolid = new G4Box( nmWall, dxyz_AC_Wall.x()/2,
				dxyz_AC_Wall.y()/2, dxyz_AC_Wall.z()/2 );
  G4Box *GasSolid = new G4Box( nmGas, dxyz_AC_Gas.x()/2,
			       dxyz_AC_Gas.y()/2, dxyz_AC_Gas.z()/2 );
  G4Box *acSolid = new G4Box( elemName_, 
			      dxyz_AC.x()/2, dxyz_AC.y()/2, dxyz_AC.z()/2 );

  // definition of Logical Volume
  G4LogicalVolume *WallLV = new G4LogicalVolume( WallSolid, mBox_, nmWall );
  G4LogicalVolume *GasLV = new G4LogicalVolume( GasSolid, mGas_, nmGas );
  G4LogicalVolume *acLV = new G4LogicalVolume( acSolid, mRad_, elemName_ );

  // establish Physical Volume
  G4VPhysicalVolume *Wall =
    new G4PVPlacement( &rotMtx_, gPos_, nmWall, WallLV,
		       pMother, false, Id_ );
  G4VPhysicalVolume *Gas = 
    new G4PVPlacement( 0, G4ThreeVector(0.,0.,0.), nmGas,
		       GasLV, Wall, false, Id_ );
//   G4VPhysicalVolume *Segment[7], *ACSegment[7];

  G4double z = -12.5*cm;
  for( int i=0; i<7; ++i ){
    G4double x = ( 60.6 -20.2*i )*cm;

//     Segment[i] = new G4PVPlacement( 0, G4ThreeVector(x,0.,0.),
// 				    nmBox, GasLV, Wall, false, i );
//     ACSegment[i] = new G4PVPlacement( 0, G4ThreeVector(0.,0.,z),elemName_, 
// 				      acLV, Segment[i], false, i );
    new G4PVPlacement( 0, G4ThreeVector(x,0.,z),
				      elemName_, acLV, Gas, false, i );
  }

  WallLV -> SetVisAttributes( G4VisAttributes::Invisible );
  GasLV -> SetVisAttributes( G4VisAttributes::Invisible );
  G4VisAttributes *acVisAtt = new G4VisAttributes();
  //new G4VisAttributes( G4Colour::G4Colour(1.,0.,1.) );
  acVisAtt->SetColour(1.0 , 0.0 , 1.0);
  acLV-> SetVisAttributes( acVisAtt );

  DetectorLV_ = acLV;
  vSolid_ = WallSolid;

  //delete acVisAtt;
  return Wall;
}
