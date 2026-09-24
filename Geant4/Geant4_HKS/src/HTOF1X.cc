/*
  HTOF1X.cc

  H.Nomura
  2003/Dec/12
*/

#include "HTOF1X.hh"

#include "G4Material.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

HTOF1X::HTOF1X( const G4String &name,
		const G4ThreeVector &gPos,
		const G4RotationMatrix *rotMtx, int id )
  : BeamLineElement( name, gPos, rotMtx ),
    mVol_(0), DetectorLV_(0), vSolid(0), Id_(id)
{
}

HTOF1X::~HTOF1X()
{
}

bool HTOF1X::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  if( vSolid-> Inside(lPos) != kOutside ) return true;
  else return false;
}


G4VPhysicalVolume *HTOF1X::Place( G4VPhysicalVolume *pMother )
{
  G4String nmWall = elemName_+" Wall";
  G4String nmSeg = elemName_+" Segment";

  G4ThreeVector dxyz_1X_Wall( 127.5*cm, 30.*cm, 2.*cm );
  G4ThreeVector dxyz_1X_Seg( 7.5*cm, 30.*cm, 2.*cm );

  G4Box *WallSolid = new G4Box( nmWall, dxyz_1X_Wall.x()/2.,
				dxyz_1X_Wall.y()/2., dxyz_1X_Wall.z()/2. );
  G4Box *SegSolid = new G4Box( nmSeg, dxyz_1X_Seg.x()/2.,
			       dxyz_1X_Seg.y()/2., dxyz_1X_Seg.z()/2. );

  G4LogicalVolume *WallLV =
    new G4LogicalVolume( WallSolid, mVol_, nmWall );
  G4LogicalVolume *SegLV =
    new G4LogicalVolume( SegSolid, mVol_, nmSeg );

  G4VPhysicalVolume *Wall = 
    new G4PVPlacement( &rotMtx_, gPos_, elemName_, WallLV,
		       pMother, false, Id_ );

  for( G4int i=0; i<17; ++i ){
    G4double x = ( 60. -7.5*i )*cm;
    G4ThreeVector posSeg( x, 0., 0. );
      new G4PVPlacement( 0, posSeg, elemName_, SegLV,
			 Wall, false, i );
  }

  WallLV-> SetVisAttributes( G4VisAttributes::Invisible );
  G4VisAttributes *VisAtt = new G4VisAttributes();
  //new G4VisAttributes( G4Colour::G4Colour(0.,1.,1.) );
  VisAtt->SetColour(0.0 , 1.0 , 1.0);
  SegLV-> SetVisAttributes( VisAtt );
  
  DetectorLV_ = SegLV;
  vSolid = WallSolid;
  //delete VisAtt;
  return Wall;
}
