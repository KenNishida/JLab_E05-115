/*
  TargetChamber.cc
  for Water Cherenkov Counter
  H.Nomura 2003/Dec/31
*/

#include "TargetChamber.hh"

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

TargetChamber::TargetChamber( const G4String &name,
			      const G4ThreeVector &gPos,
			      const G4RotationMatrix *rotMtx)
  : BeamLineElement(name,gPos,rotMtx),
    mBox_(0), mGap_(0), yokeLV_(0), vSolid_(0)
{
}

TargetChamber::~TargetChamber()
{
}

bool TargetChamber::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  if( vSolid_-> Inside(lPos) != kOutside ) return true;
  else return false;
}

G4VPhysicalVolume *TargetChamber::Place( G4VPhysicalVolume *pMother )
{
  G4String nmGap = elemName_+" Gap";
  G4String nmBox = elemName_+" Box";

  G4ThreeVector dxyz_Out( 23.55*cm, 17.14*cm, 7.9*cm);
  G4ThreeVector dxyz_In( 21.01*cm, 14.6*cm, 7.9*cm );


  // definition of solid
  G4Box *OutBox = new G4Box( nmBox,
			     dxyz_Out.x()/2,
			     dxyz_Out.y()/2,
			     dxyz_Out.z()/2 );
  G4Box *InBox = new G4Box( nmGap,
			    dxyz_In.x()/2,
			    dxyz_In.y()/2,
			    dxyz_In.z()/2 );

  G4SubtractionSolid *TCSolid =
    new G4SubtractionSolid( elemName_+" Solid",
			    OutBox, InBox );

  // definition of Logical Volume
  G4LogicalVolume *TCLV =
    new G4LogicalVolume( TCSolid, mBox_, elemName_+" LV" );

  G4LogicalVolume *TCgap =
    new G4LogicalVolume( InBox, mGap_, nmGap+" LV" );

  // establish Physical Volume
  G4VPhysicalVolume *TChamber = 
    new G4PVPlacement( &rotMtx_, gPos_, elemName_,
		       TCLV, pMother, false, 0 );

  G4RotationMatrix *rotTMP = new G4RotationMatrix();
  new G4PVPlacement( rotTMP, G4ThreeVector(0.,0.,0.),
		     TCgap, nmGap, TCLV, false, 0 );

  ///////////////////
  // Visualisation //
  ///////////////////
  //TCLV-> SetVisAttributes( G4VisAttributes::Invisible );

  vSolid_ = InBox;
  yokeLV_ = TCLV;

  return TChamber;
}

void TargetChamber::SetUserLimits( G4UserLimits *limitYoke )
{
  if( yokeLV_ ) yokeLV_->SetUserLimits( limitYoke );
}
