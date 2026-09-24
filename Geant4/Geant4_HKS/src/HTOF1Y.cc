/*
  HTOF1Y.cc

  H.Nomura
  2003/Dec/12
*/

#include "HTOF1Y.hh"

#include "G4Material.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

HTOF1Y::HTOF1Y( const G4String &name,
		const G4ThreeVector &gPos,
		const G4RotationMatrix *rotMtx, int id )
  : BeamLineElement( name, gPos, rotMtx ),
    mVol_(0), DetectorLV_(0), vSolid_(0), Id_(id)
{
}

HTOF1Y::~HTOF1Y()
{
}

bool HTOF1Y::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition( gPos );
  if( vSolid_-> Inside(lPos) != kOutside ) return true;
  else return false;
}


G4VPhysicalVolume *HTOF1Y::Place( G4VPhysicalVolume *pMother )
{
  G4String nmWall = elemName_+" Wall";
  G4String nmSeg = elemName_+" Segment";
  G4double d = 0.5;
  //d=overlap  

  //G4ThreeVector dxyz_1Y_Wall( 125.*cm, 31.5*cm, 4.*cm );
  G4ThreeVector dxyz_1Y_Wall( 125.*cm, (31.5-d*8.)*cm, 8.5*cm );
  G4ThreeVector dxyz_1Y_Seg( 125.*cm, 3.5*cm, 2.*cm );

  G4Box *WallSolid = new G4Box( nmWall, dxyz_1Y_Wall.x()/2,
				dxyz_1Y_Wall.y()/2, dxyz_1Y_Wall.z()/2 );
  G4Box *SegSolid = new G4Box( nmSeg, dxyz_1Y_Seg.x()/2,
			       dxyz_1Y_Seg.y()/2, dxyz_1Y_Seg.z()/2 );

  G4LogicalVolume *WallLV =
    new G4LogicalVolume( WallSolid, mVol_, nmWall );
  G4LogicalVolume *SegLV =
    new G4LogicalVolume( SegSolid, mVol_, nmSeg );

  G4VPhysicalVolume *Wall = 
    new G4PVPlacement( &rotMtx_, gPos_, elemName_, WallLV,
		       pMother, false, Id_ );

  for( int i=0; i<9; ++i ){
    //G4double y = ( 14.-3.5*i )*cm;
    G4double y = dxyz_1Y_Wall.y()/2.-3.5/2*cm-(3.5-d)*i*cm;
    G4double z1 =  -3.25*cm;
    G4double z2 =   3.25*cm;
    G4ThreeVector posSeg;
    if (i%2==0){
      posSeg=G4ThreeVector( 0., y, z1 );
    }
    else {
      posSeg=G4ThreeVector( 0., y, z2 );
    }

      new G4PVPlacement( 0, posSeg, elemName_, SegLV,
			 Wall, false, i );
  }

  WallLV-> SetVisAttributes( G4VisAttributes::Invisible );
  G4VisAttributes *VisAtt = new G4VisAttributes();
    //new G4VisAttributes( G4Colour::G4Colour(0.,1.,1.) );
  VisAtt->SetColour(0.0 , 1.0 , 1.0);
  SegLV-> SetVisAttributes( VisAtt );
  
  DetectorLV_ = SegLV;
  vSolid_ = WallSolid;
  //delete VisAtt;
  return Wall;
}
