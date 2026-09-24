/*
  ChamberExtension.cc

  H.Nomura
  2003/Dec/5
*/

#include "ChamberExtension2.hh"

#include "G4Box.hh"
#include "G4SubtractionSolid.hh"
#include "G4DisplacedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include <cmath>

const std::string deffilename = "param/FringeField.map";

ChamberExtension2::ChamberExtension2( const G4String &name,
				    const G4ThreeVector &gPos,
				    const G4RotationMatrix *rotMtx )
  : BeamLineElement( name, gPos, rotMtx ),
    solidField(0), gapLV_(0), yokeLV_(0)
{
}

ChamberExtension2::~ChamberExtension2()
{
}

bool ChamberExtension2::ExistMagneticField( void ) const
{
  return false;
}

bool ChamberExtension2::IsInside( const G4ThreeVector &gPos ) const
{
  G4ThreeVector lPos = GetLocalPosition( gPos );
  if( solidField-> Inside(lPos)!=kOutside ) return true;
  else return false;
}


G4VPhysicalVolume *ChamberExtension2::Place( G4VPhysicalVolume *pMother )
{
  G4String nmBox = elemName_+" Box";
  G4String nmOut = elemName_+" Out";
  G4String nmGap = elemName_+" Gap";
  //G4String nmWind = elemName_+" Window";
  
  //G4ThreeVector dxyz_CE_Out( 147.4*cm, 35.4*cm, 140.*cm );
  G4ThreeVector dxyz_CE_Out( 133.2*cm, 21.20*cm, 35.*cm );
  G4ThreeVector dxyz_CE_In( 131.2*cm, 20.0*cm, 35.*cm );
  //G4ThreeVector dxyz_Wind( 132.*cm, 21.2*cm, 0.02*cm );
  G4Box *Box = new G4Box( nmBox, dxyz_CE_Out.x()/2,
			  dxyz_CE_Out.y()/2, dxyz_CE_Out.z()/2 );
  G4Box *Gap = new G4Box( nmGap, dxyz_CE_In.x()/2,
			  dxyz_CE_In.y()/2, dxyz_CE_In.z()/2 );
  //*Window = new G4Box( nmWind, dxyz_Wind.x()/2,
  //		     dxyz_Wind.y()/2, dxyz_Wind.z()/2 );
  

  G4ThreeVector move( 266.3*cm, 0.*cm, 17.5*cm );
  G4DisplacedSolid *CeBox = 
    new G4DisplacedSolid( nmBox, Box, 0, move );
  G4DisplacedSolid *GapSolid = 
    new G4DisplacedSolid( nmGap, Gap, 0, move );

  G4SubtractionSolid *CExtensionSolid =
    new G4SubtractionSolid( nmOut, CeBox, GapSolid );

  G4LogicalVolume *CExtensionLV =
    new G4LogicalVolume( CExtensionSolid, mYoke_, elemName_+" LV" );
  G4LogicalVolume *GapLV =
    new G4LogicalVolume( GapSolid, mGap_, nmGap+" LV" );
  //G4LogicalVolume *WindowLV =
  //  new G4LogicalVolume( Window, mWind_, nmWind );
  //
  //G4double posz = 69.99*cm;
  
  G4VPhysicalVolume *CExtension =
    new G4PVPlacement( &rotMtx_, gPos_, elemName_+" PV",
		   CExtensionLV, pMother, false, 0 );
  G4PVPlacement *GapPV =
    new G4PVPlacement( 0, G4ThreeVector(), nmGap+" PV",
		   GapLV, CExtension, false, 0 );
  //G4PVPlacement *WindPV =
  //  new G4PVPlacement( 0, G4ThreeVector(0.,0.,posz), nmWind+" PV",
  //		       WindowLV, GapPV, false, 0 );
  
  ///////////////////
  // Visualisation //
  ///////////////////
  //G4Colour colourCE(0.5, 0.5, 0.5); //grey
  //G4Colour colourCE(0.7, 0.7, 0.8); //grey
  G4Colour colourCE(0.7, 0.3, 0.3); //
  G4VisAttributes *CExtensionVisAtt = 
    new G4VisAttributes( true, colourCE );
  CExtensionLV-> SetVisAttributes( CExtensionVisAtt );
  //WindowLV-> SetVisAttributes( G4VisAttributes::Invisible );

  solidField = GapSolid;
  gapLV_ = GapLV;
  yokeLV_ = CExtensionLV;

  return CExtension;
}

void ChamberExtension2::SetUserLimits( G4UserLimits *limitYoke,
				      G4UserLimits *limitGap )
{
  if( yokeLV_ ) yokeLV_-> SetUserLimits( limitYoke );
  if( gapLV_&&limitGap )
    gapLV_-> SetUserLimits( limitGap );
}

