/*
  SieveSlit.cc
  2004/Sep/21 H.Nomura
*/

#include "SieveSlit.hh"

#include "G4Box.hh"
#include "G4Tubs.hh"

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"

#include <cmath>

//////////////////////////////////////////////////////
SieveSlit::SieveSlit( const G4String &name,
		      const G4ThreeVector &gPos,
		      const G4RotationMatrix *rotMtx )
  : BeamLineElement( name, gPos, rotMtx ),
    mBox_(0), mHole_(0)
//////////////////////////////////////////////////////
{
}

///////////////////////
SieveSlit::~SieveSlit()
///////////////////////
{
}

///////////////////////////////////////////////////////////
bool SieveSlit::IsInside( const G4ThreeVector &gPos ) const
///////////////////////////////////////////////////////////
{
  G4ThreeVector lPos = GetLocalPosition( gPos );
  if( vSolid_->Inside(lPos) != kOutside ) return true;
  else return false;
}

///////////////////////////////////////////////////////////////
G4VPhysicalVolume *SieveSlit::Place(G4VPhysicalVolume *pMother)
///////////////////////////////////////////////////////////////
{
  G4String nmHole = elemName_+" hole";

  G4ThreeVector dxyzBox(22.*cm, 10.*cm, 2.54*cm);
  G4Box *SlitBox =
    new G4Box( "SS solid", dxyzBox.x(),
	       dxyzBox.y(), dxyzBox.z() );
  G4Tubs *HOLE = new G4Tubs( "hole", 0.*cm, 0.249/2.*2.54*cm,
			     3.*cm, 0.*degree, 360.*degree );
  G4Tubs *HOLE0 = new G4Tubs( "hole", 0.*cm, 1./8./2.*2.54*cm,
			     3.*cm, 0.*degree, 360.*degree );
  G4DisplacedSolid *holeSolid;
  G4SubtractionSolid *TmpSolid[53];
  G4double holeX[53]={
			 -7.620,-7.620,-7.620,
			 -6.096,-6.096,-6.096,
			 -4.572,-4.572,-4.572,-4.572,-4.572,
			 -3.048,-3.048,-3.048,-3.048,-3.048,-3.048,-3.048,
			 -1.524,-1.524,-1.524,-1.524,-1.524,-1.524,-1.524,
			  0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
			  1.524, 1.524, 1.524, 1.524, 1.524, 1.524,
			  3.048, 3.048, 3.048, 3.048, 3.048, 3.048, 3.048,
			  4.572, 4.572, 4.572, 4.572, 4.572,
			  6.096, 6.096, 6.096
  };
  G4double holeY[53]={
 			2.032,0,-2.032, 		
 			2.032,0,-2.032, 		
 			4.064,2.032,0,-2.032,-4.064, 		
 			6.096,4.064,2.032,0,-2.032,-4.064,-6.096, 		
 			6.096,4.064,2.032,0,-2.032,-4.064,-6.096,		
 			6.096,4.064,2.032,0,-2.032,-4.064,-6.096, 		
 			6.096,4.064,2.032,0,-4.064,-6.096, 		
 			6.096,4.064,2.032,0,-2.032,-4.064,-6.096, 		
 			4.064,2.032,0,-2.032,-4.064, 		
 			2.032,0,-2.032 		
  };
  
  G4double d = 85.*cm;
  for( G4int i=0; i<53; i++ ){
      /*G4int Column = i%8;
      G4int Row = G4int( i/8 );
      G4double x =(0.6*2.54*(Column-4)+1.)*cm;
      G4double y =(0.8*2.54*(Row-4)+1.)*cm;*/
      G4double x = holeX[i]*cm;
      G4double y = holeY[i]*cm;
      G4double theta = -atan2( x/(d-dxyzBox.z()), 1. );
      G4double phi = atan2( y/(d-dxyzBox.z()), 1. );
      //G4double theta = 0.;
      //G4double phi = 0.;
      G4RotationMatrix *rotH = new G4RotationMatrix();
      rotH->rotateY(theta);
      rotH->rotateX(phi);
      if (i==29){
			holeSolid = 
			new G4DisplacedSolid( nmHole+" solid", HOLE0, rotH,
				              G4ThreeVector(x,y,0.) );
		}
		else{
      	holeSolid = 
			new G4DisplacedSolid( nmHole+" solid", HOLE, rotH,
		    				          G4ThreeVector(x,y,0.) );
		}
      if (i==0){
        TmpSolid[i] = 
         new G4SubtractionSolid("TmpSolid", SlitBox, holeSolid);
      }
      else {
        TmpSolid[i] = 
         new G4SubtractionSolid("TmpSolid", TmpSolid[i-1], holeSolid);
      }
    /*}
    else{
      holeSolid[i] =
	new G4Tubs( nmHole+" solid", 0.*cm, 0.15875*cm, dxyzBox.z(),
		    0.*degree, 360.*degree );
    }*/
  }


  G4LogicalVolume *SlitLV =
    new G4LogicalVolume( TmpSolid[52], mBox_, "SSLV" );
  //  G4LogicalVolume *holeLV[70];
  G4VPhysicalVolume *Slit =
    new G4PVPlacement( &rotMtx_, gPos_, "SS",
		       SlitLV, pMother, false, 0 );

  ///////////////////
  // Visualisation //
  ///////////////////
  G4Colour colourSlit(0.5, 0.5, 0.5); // gray
  G4VisAttributes* SlitVisAtt = new G4VisAttributes(true, colourSlit);
  SlitLV-> SetVisAttributes(SlitVisAtt);
  //SlitLV-> SetVisAttributes(G4VisAttributes::Invisible);

  vSolid_ = SlitBox;
//   logDetector1_ = holeLV[0];
//   logDetector2_ = holeLV[1];

  delete SlitVisAtt;
  return Slit;
}

//////////////////////////////////////////////////////////
void SieveSlit::SetSD( G4VSensitiveDetector *pSDetector )
//////////////////////////////////////////////////////////
{
  for( G4int i=0; i<70; i++ ){
    holeLV[i]->SetSensitiveDetector( pSDetector );
  }
}
