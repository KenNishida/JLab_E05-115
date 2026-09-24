/*
  DriftChamber.cc

  2003/Dec/12
  H.Nomura
*/

#include "DriftChamber.hh"

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"
#include "G4DisplacedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include <cmath>

DriftChamber::DriftChamber( const G4String &name,
			    const G4ThreeVector &gPos,
			    const G4RotationMatrix *rotMtx,
			    int id )
  : BeamLineElement( name, gPos, rotMtx ),
    mBox_(0), mGas_(0), mMylar_(0), mLayer_(0),
    DetectorLV_(0), vSolid_(0)
{
  Id_ = id;
}

DriftChamber::~DriftChamber()
{
}

bool DriftChamber::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition( gPos );
  if( vSolid_->Inside(lPos) != kOutside ) return true;
  else return false;
}

G4VPhysicalVolume *DriftChamber::Place( G4VPhysicalVolume *pMother )
{
  G4String nmBox = elemName_+" Box";
  G4String nmMylar = elemName_+" Mylar";
  G4String nmCu = "Cu";
  G4String nmGas = elemName_+" Gas";
  G4String nmLayer = elemName_+" Layer";


  G4ThreeVector dxyz_Box( 130.*cm, 30.5*cm, 5.13*cm );
  G4ThreeVector dxyz_Mylar(122.*cm, 30.5*cm, 0.0012*cm );
  G4ThreeVector dxyz_Gas( 122.*cm, 30.5*cm, 5.13*cm );
  G4ThreeVector dxyz_Layer( 122.*cm, 30.5*cm, 0.1*cm);

  G4Box *BoxSolid = new G4Box( nmBox, dxyz_Box.x()/2,
			       dxyz_Box.y()/2, dxyz_Box.z()/2 );
  G4Box *MylarSolid = new G4Box( nmMylar, dxyz_Mylar.x()/2,
				dxyz_Mylar.y()/2, dxyz_Mylar.z()/2 );
  G4Box *GasSolid = new G4Box( nmGas, dxyz_Gas.x()/2,
			       dxyz_Gas.y()/2, dxyz_Gas.z()/2 );
  G4Box *LayerSolid = new G4Box( nmLayer, dxyz_Layer.x()/2,
				 dxyz_Layer.y()/2, dxyz_Layer.z()/2 );


  G4LogicalVolume *BoxLV =
    new G4LogicalVolume( BoxSolid, mBox_, elemName_ );
  G4LogicalVolume *MylarLV = 
    new G4LogicalVolume( MylarSolid, mMylar_, nmMylar );
  G4LogicalVolume *GasLV =
    new G4LogicalVolume( GasSolid, mGas_, nmGas );
  G4LogicalVolume *LayerLV =
    new G4LogicalVolume( LayerSolid, mLayer_, nmGas );

  G4VPhysicalVolume *DCBox = 
    new G4PVPlacement( &rotMtx_, gPos_, nmBox, BoxLV,
		       pMother, false, Id_ );
  G4VPhysicalVolume *DCGas = 
    new G4PVPlacement( 0, G4ThreeVector(), nmGas, GasLV, 
		       DCBox, false, Id_ );

  G4double z;
//   G4int ID = Id_+1;

  // DC-Layer
  for( G4int i=0; i<3; i++ ){
    z = ( -1.905 +0.635*i )*cm;
    G4ThreeVector posLayer( 0.*cm, 0.*cm, z );
    new G4PVPlacement( 0, posLayer, elemName_,
				    LayerLV, DCGas, false, i );
  }
  for( G4int i=3; i<6; i++ ){
    z = ( 0.3175 + 0.635*(i-3) )*cm;
    G4ThreeVector posLayer( 0.*cm, 0.*cm, z );
    new G4PVPlacement( 0, posLayer, elemName_,
				    LayerLV, DCGas, false, i );
  }
  // DC-Mayer
  z = -2.54*cm;
  new G4PVPlacement( 0, 
				  G4ThreeVector( 0.*cm, 0.*cm, z ),
				  nmMylar, MylarLV, DCGas, false, 0 );
  for( G4int i=1; i<9; i++ ){
    z = (-2.225+0.635*(i-1))*cm;
    new G4PVPlacement( 0, G4ThreeVector(0.*cm,0.*cm,z),
				    nmMylar, MylarLV, DCGas, false, i );
  }
  z = 2.54*cm;
  new G4PVPlacement( 0,
				  G4ThreeVector( 0.*cm, 0.*cm, z ),
				  nmMylar, MylarLV, DCGas, false, 9 );

  G4VisAttributes *DCVis = new G4VisAttributes();
    //new G4VisAttributes( G4Colour::G4Colour(0.,1.,0.) );
  DCVis->SetColour(0.0 , 1.0 , 0.0);
  BoxLV-> SetVisAttributes( DCVis );
  GasLV-> SetVisAttributes( G4VisAttributes::Invisible );
  MylarLV-> SetVisAttributes( G4VisAttributes::Invisible );
  LayerLV-> SetVisAttributes( G4VisAttributes::Invisible );

  DetectorLV_ = LayerLV;
  vSolid_ = BoxSolid;
  return DCBox;
}
