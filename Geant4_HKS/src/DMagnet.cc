/*
  DMagnet.cc
  for 70-deg dipole magnet

  M.Kawai
*/

#include "DMagnet.hh"
#include "DFieldMan.hh"

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"
#include "G4DisplacedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "HKSParamMan.hh"

#include <cmath>


//////////////////////////////////////////////////////////////////
DMagnet::DMagnet( const G4String &name, const G4ThreeVector &gPos,
		  const G4RotationMatrix *rotMtx,
		  const std::string deffilename )
  : BeamLineElement( name, gPos, rotMtx ),
    solidField(0), gapLV_(0), yokeLV_(0), DetectorLV_(0),
    fMan(deffilename)

{
  fMan.Initialize();
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  Scale = paramMan->GetDScaleFactor();
}

///////////////////
DMagnet::~DMagnet()
///////////////////
{
}

//////////////////////////////////////////////
bool DMagnet::ExistMagneticField( void ) const
//////////////////////////////////////////////
{
  return true;
}


bool DMagnet::IsInside( const G4ThreeVector &gPos ) const
/////////////////////////////////////////////////////////
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  //if( solidField->Inside(lPos)!=kOutside ) return true;
    /*if( lPos.x()>=0.*cm && lPos.x()<=500.*cm &&
        lPos.y()>=-12.*cm && lPos.y()<=12.*cm &&
        lPos.z()>=-100.*cm && lPos.z()<=400.*cm 
    )*/
    if( lPos.x()>=0.*cm && lPos.x()<=500.*cm &&
        lPos.y()>=-12.*cm && lPos.y()<=12.*cm &&
        lPos.z()>=-300.*cm && lPos.z()<=300.*cm 
        //lPos.z()>=-400.*cm && lPos.z()<=400.*cm 
    )
  {  
    return true;
  }
  else return false; 
}

/////////////////////////////////////////////////////////////////////
G4ThreeVector DMagnet::GetMagneticField( const G4ThreeVector &gPos ) const
/////////////////////////////////////////////////////////////////////
{
  if( IsInside(gPos) ){
    G4ThreeVector lPos = GetLocalPosition(gPos);
    return GetGlobalDirection( Field(lPos) );
  }
  else return G4ThreeVector();
}

///////////////////////////////////////////////////////////////
G4ThreeVector DMagnet::Field( const G4ThreeVector &pos ) const
//////////////////////////////////////////////////////////////
{
  double X[3];
  double Bfield[3];
  int Xflag = 1;
  int Yflag = 1;
  int Zflag = 1;
  X[0] = pos.x()/cm;
  X[1] = pos.y()/cm;
  X[2] = pos.z()/cm;

  if ( fMan.GetFieldValue(X,Bfield) ){
    Bfield[0] *= Xflag*Scale*gauss;
    Bfield[1] *= Yflag*Scale*gauss;
    Bfield[2] *= Zflag*Scale*gauss;
  }
  else {
    Bfield[0] = Bfield[1] = Bfield[2] = 0;
  }
  

  G4ThreeVector B( Bfield[0], Bfield[1], Bfield[2] );
  return B;
}

////////////////////////////////////////////////////////////////
G4VPhysicalVolume *DMagnet::Place( G4VPhysicalVolume *pMother )
////////////////////////////////////////////////////////////////
{
  G4String nmLV = elemName_+" LV";
  G4String nmPV = "D";
  G4String nmTubs = elemName_+" Tubs";
  G4String nmGap = elemName_+" Gap";
  G4RotationMatrix *rotSolid = new G4RotationMatrix();
  rotSolid-> rotateX(90.*degree);

  G4double InRad = (53.0/sin(35.*degree))*cm;
  G4double OutRad = 450.0*cm;
  G4double Bend = 70.*degree;
  G4double SBend = -35.*degree;
  //G4double SBend = -70.*degree;
  //G4double GapCen = 270.*cm;
  //  G4double PoleGap = 20.*cm;
  //G4double GapWid = 166.*cm;
  //G4double PoleGap = 15.*cm;
  //G4double GapWid = 40.*cm;
  
  G4Tubs *Tubs =
    new G4Tubs( "Tubs", InRad, OutRad, 125.*cm,
		SBend, Bend);
  G4DisplacedSolid *DTubs = 
    new G4DisplacedSolid( nmTubs, Tubs, rotSolid,
			  G4ThreeVector(0., 0., 0.) );

  G4Box *fieldBox =
    new G4Box( "Field", 200.*cm, 15.*cm, 250.*cm );

  G4Tubs *Gap = 
    new G4Tubs( "Gap", 190.5*cm, 349.5*cm,
		10.0*cm, SBend, Bend);
  G4DisplacedSolid *DGapSolid =
    new G4DisplacedSolid( nmGap, Gap, rotSolid,
			  G4ThreeVector(0., 0., 0.) );

  /*G4Tubs *VD = 
    new G4Tubs( "VD", GapCen-GapWid/2, GapCen+GapWid/2,
		20.0*cm, SBend, 5.*degree);
  G4DisplacedSolid *VDsolid =
    new G4DisplacedSolid( "VD", VD, rotSolid,
			  G4ThreeVector(0., 0., 0.) );*/

  G4SubtractionSolid *DSolid =
    new G4SubtractionSolid( elemName_+" Solid", DTubs, DGapSolid );

  G4LogicalVolume *DMagnetLV = 
    new G4LogicalVolume( DSolid, mYoke_, nmLV );
  /*G4LogicalVolume *DGapLV =
    new G4LogicalVolume( DGapSolid, mGap_, "D Gap LV" );
  G4LogicalVolume *VDLV = 
    new G4LogicalVolume( VDsolid, mGap_, "VD LV" );*/

  //G4VPhysicalVolume *DMagnet =
  // new G4PVPlacement( &, gPos_, nmPV, DMagnetLV,
  //		       pMother, false, 0 );
  G4VPhysicalVolume *DMagnet =
    new G4PVPlacement( &rotMtx_, gPos_, "D", DMagnetLV,
		       pMother, false, 0 );
  
  //  G4VPhysicalVolume *DGap =
  //new G4PVPlacement( &rotMtx_, G4ThreeVector(0., 0., 0.),
  //	       "D Gap PV", DGapLV, DMagnet, false, 0 );

  //G4VPhysicalVolume *DGap =
  //  new G4PVPlacement( &rotMtx_, gPos_,
  //		       "D Gap PV", DGapLV, pMother, false, 0 );

  /*G4VPhysicalVolume *DipoleVD[6];
  for( G4int i=0; i<4; i++ ){
    G4RotationMatrix *rotVD = new G4RotationMatrix();
    G4double rot = -( 5. + i*10. )*degree;
    rotVD->rotateY( rot );
    G4int Id = i+6;
    DipoleVD[i] = new G4PVPlacement( rotVD, G4ThreeVector(),
				     "D PV", VDLV, DGap, false, Id );
  }*/

  ///////////////////
  // Visualisation //
  ///////////////////
  G4Colour colourDYoke(0., 1., 0.); // magenta
  G4VisAttributes* DYokeVisAtt = new G4VisAttributes(true, colourDYoke);
  DMagnetLV-> SetVisAttributes(DYokeVisAtt);
  //VDLV->SetVisAttributes(G4VisAttributes::Invisible);

  G4ThreeVector sField( 250.*cm, 0.*cm, -150.*cm );
  solidField = new G4DisplacedSolid( "D field", fieldBox,
				     0, sField );
  //yokeLV_ = DMagnetLV;
  //gapLV_ = DGapLV;
  //DetectorLV_ = VDLV;
  
  delete rotSolid;
  return DMagnet;
}

//////////////////////////////////////////////////////
void DMagnet::SetUserLimits( G4UserLimits *limitYoke,
			     G4UserLimits *limitGap )
//////////////////////////////////////////////////////
{
  if( yokeLV_ ) yokeLV_-> SetUserLimits( limitYoke );
  if( gapLV_&&limitGap )
    gapLV_-> SetUserLimits( limitGap );
}
