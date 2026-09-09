/*
  Collimator.cc
  M.Kawai
*/

#include "Collimator.hh"

#include "G4Material.hh"
#include "G4Box.hh"
#include "G4Trd.hh"
#include "G4SubtractionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4DisplacedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

Collimator::Collimator( const G4String &name,
			const G4ThreeVector &gPos,
			const G4RotationMatrix *rotMtx,
			int id )
  : BeamLineElement(name,gPos,rotMtx),
    mBox_(0), mGap_(0), DetectorLV_(0), vSolid_(0)
{
  Id_ = id;
}

Collimator::~Collimator()
{
}

bool Collimator::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  if( vSolid_-> Inside(lPos) != kOutside ) return true;
  else return false;
}

G4VPhysicalVolume *Collimator::Place( G4VPhysicalVolume *pMother )
{
  G4String nmGap = elemName_+" Gap";
  G4String nmBox = elemName_+" Box";

  G4ThreeVector dxyz_Col_Box( 22.0*cm, 8.0*2.54/2*cm, 2.54*cm );

  // definition of solid
  G4Box *ColBoxSolid = new G4Box( nmBox, 
				  dxyz_Col_Box.x(),
				  dxyz_Col_Box.y(),
				  dxyz_Col_Box.z() );
  //G4DisplacedSolid *ColGapSolid = new Collimator::MakeGap( nmGap );

  //  G4SubtractionSolid *BoxSolid =
  //new G4SubtractionSolid( "Box", ColBoxSolid, ColGapSolid );
  G4double thickness = 2.55*2.*cm;
  G4Box *BoxA = new G4Box( "Col Box 1", 2.124*2.54/2.*cm, 
  									5.944*2.54/2.*cm, thickness/2.);
  G4Trd *TrdA = new G4Trd( "Col Trd 1", thickness/2., thickness/2.,
			   5.944*2.54/2.*cm, 2.124*2.54/2.*cm, 2.335*2.54/2.*cm );
  G4Trd *TrdB = new G4Trd( "Col Trd 2", thickness/2., thickness/2.,
			   5.944*2.54/2.*cm, 2.124*2.54/2.*cm, 1.91*2.54/2.*cm );

  G4RotationMatrix *rotR = new G4RotationMatrix();
  rotR->rotateY(90*degree);
  G4RotationMatrix *rotL = new G4RotationMatrix();
  rotL->rotateY(-90*degree);
  
  G4DisplacedSolid *GapA1 = 
    new G4DisplacedSolid( "Gap A1", TrdA, rotR, 
			  G4ThreeVector( -2.2295*2.54*cm,0.,0.) );
  G4DisplacedSolid *GapA2 = 
    new G4DisplacedSolid( "Gap A2", TrdB, rotL, 
			  G4ThreeVector( 2.017*2.54*cm,0.,0.) );


  G4SubtractionSolid* Gap1 =
    new G4SubtractionSolid( "Gap1", ColBoxSolid, BoxA );
  G4SubtractionSolid* Gap2 =
    new G4SubtractionSolid( "Gap2", Gap1, GapA1 );
  G4SubtractionSolid* Gap3 =
    new G4SubtractionSolid( "Gap3", Gap2, GapA2 );

  //G4SubtractionSolid* Q1Yoke3 =
  //  new G4SubtractionSolid( "Q1 Gap3", Q1Yoke2, TrdL );

  //G4IntersectionSolid *Gap =
  //new G4IntersectionSolid( name, GapBox, GapB );
  //   G4RotationMatrix *rotGap = new G4RotationMatrix();
  //   rotGap->rotateZ(180.0*degree)
  

  // definition of Logical Volume
  // G4LogicalVolume *ColBoxLV =
  //  new G4LogicalVolume( ColBoxSolid, mBox_, nmBox );
  G4LogicalVolume *ColBoxLV =
    new G4LogicalVolume( Gap3, mBox_, nmBox );
 
  //G4LogicalVolume *ColGapLV =
  //  new G4LogicalVolume( GapB, mGap_, nmGap );

  // establish Physical Volume
  G4VPhysicalVolume *ColBox = 
    new G4PVPlacement( &rotMtx_, gPos_, "Colbox",
  		       ColBoxLV, pMother, false, 0 );
  //G4VPhysicalVolume *ColGap =
  //  new G4PVPlacement( 0, G4ThreeVector(0.,0.,0.), nmGap,
  //	       ColGapLV, ColBox, false, Id_ );
  
  //G4VPhysicalVolume *ColGap =
  // new G4PVPlacement( &rotMtx_, gPos_, "ColGap",
  //	       ColGapLV, pMother, false, 1 );
 
  ///////////////////
  // Visualisation //
  ///////////////////
  
  G4Colour colour(0.5, 0.5, 0.5); // gray
  G4VisAttributes* ColVisAtt = new G4VisAttributes(true, colour);
  ColBoxLV-> SetVisAttributes(ColVisAtt);
  //ColGapLV-> SetVisAttributes( G4VisAttributes::Invisible );

  //DetectorLV_ = ColGapLV;
  DetectorLV_ = ColBoxLV;
  //vSolid_ = Gap2;
  //vSolid_ = ColGapSolid;

  return ColBox;
}
