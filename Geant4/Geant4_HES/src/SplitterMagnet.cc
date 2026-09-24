/*
  SplitterMagnet.cc

  D.Kawama
*/

#include "SplitterMagnet.hh"
#include "SplitterFieldMan.hh"

#include "G4Box.hh"
#include "G4SubtractionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "HESParamMan.hh"

#include <cmath>

//#define CFIELD 1
// #undef CFIELD

//const std::string deffilename = "param/HES_SPL_A.map";
//HESParamMan *paramMan = HESParamMan::GetParamMan();
//const std::string deffilename = paramMan->GetSplitterFieldMap();

///////////////////////////////////////////////////////////////////
SplitterMagnet::SplitterMagnet( const G4String &name,
				const G4ThreeVector &gPos,
				const G4RotationMatrix *rotMtx, 
				const std::string deffilename )
  : BeamLineElement( name, gPos, rotMtx ),
    solidField(0), gapLog_(0), yokeLog_(0),
    fMan(deffilename)
///////////////////////////////////////////////////////////////////    
{
  fMan.Initialize();
  HESParamMan *paramMan = HESParamMan::GetParamMan();
  Scale = paramMan->GetSplitterScaleFactor();
}

/////////////////////////////////
SplitterMagnet::~SplitterMagnet()
/////////////////////////////////
{
}

/////////////////////////////////////////////////////
bool SplitterMagnet::ExistMagneticField( void ) const
/////////////////////////////////////////////////////
{
  return true;
  //  return false;
}

////////////////////////////////////////////////////////////////
bool SplitterMagnet::IsInside( const G4ThreeVector &gPos ) const
////////////////////////////////////////////////////////////////
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
  //G4double radius = sqrt((lPos.x()/cm-36.55)*(lPos.x()-36.55/cm)+lPos.z()*lPos.z());
  //G4double theta = atan((lPos.z()/cm)/(lPos.x()/cm-36.55))*180./3.14;
  //if( radius<130. &&theta<82.) return true;
  if( lPos.x()>-150.*cm && lPos.x()<200.*cm &&
      lPos.y()>-50.*cm && lPos.y()<50.0*cm  &&
      //lPos.z()>-50.0*cm && lPos.z()<150.0*cm ) return true;
      lPos.z()>-50.0*cm && lPos.z()<200.0*cm ) return true;
  else return false;
}

////////////////////////////////////////////////////
G4ThreeVector SplitterMagnet::
GetMagneticField( const G4ThreeVector &gPos ) const
////////////////////////////////////////////////////
{
  if( IsInside(gPos) ){
    G4ThreeVector lPos = GetLocalPosition( gPos );
    return GetGlobalDirection( Field( lPos ) );
  }
  else return G4ThreeVector();

}

/////////////////////////////////////////////////////////////////////
G4ThreeVector SplitterMagnet::Field( const G4ThreeVector &pos ) const
/////////////////////////////////////////////////////////////////////
{ 
  double X[3];
  double Bfield[3];
  int Xflag = 1;
  int Yflag = 1;
  int Zflag = 1;
   X[0] = pos.x()/cm;
   X[1] = pos.y()/cm;
   X[2] = pos.z()/cm;

  if( fMan.GetFieldValue(X, Bfield) ){
    Bfield[0] *= Scale*Xflag*gauss;
    Bfield[1] *= Scale*Yflag*gauss;
    Bfield[2] *= Scale*Zflag*gauss;
  }
  else{
    Bfield[0] = Bfield[1] = Bfield[2] = 0;
  }

  G4ThreeVector B( Bfield[0], Bfield[1], Bfield[2] );
  //G4ThreeVector B( 0.*gauss, -15955.*gauss, 0.*gauss );
  return B;

}

////////////////////////////////////////////////////
G4VPhysicalVolume*
SplitterMagnet::Place( G4VPhysicalVolume *pMother )
////////////////////////////////////////////////////
{
  G4ThreeVector dxyz_SplitY( 229.5*cm, 159.0*cm, 89.0*cm);
  G4ThreeVector dxyz_SplitG( 128.0*cm,  34.0*cm,  89.0*cm);
  G4Box* SplitterBox = new G4Box( "Splitter Box w/o Gap",
				  dxyz_SplitY.x()/2,
				  dxyz_SplitY.y()/2,
				  dxyz_SplitY.z()/2);
  G4Box* SplitterGap = new G4Box( "Splitter Gap",
				  dxyz_SplitG.x()/2,
				  dxyz_SplitG.y()/2,
				  dxyz_SplitG.z()/2);
  G4double SpGap = 0;
  G4ThreeVector gap( SpGap, 0.*cm, 0.*cm);
  G4Box *fieldBox = new G4Box( "Field Solid",
			       175.*cm, 50.*cm, 100.*cm );
  G4ThreeVector sField( 0.*cm, 0.*cm, 0.*cm );


  G4SubtractionSolid* splitterSolid =
    new G4SubtractionSolid( "Splitter Yoke",
			    SplitterBox, SplitterGap, 0, gap );
  G4DisplacedSolid* Splitter =
    new G4DisplacedSolid("Splitter", splitterSolid, 0, 
                         G4ThreeVector(0.*cm,0.*cm,44.5*cm));

  G4LogicalVolume* splitterLV =
    new G4LogicalVolume( Splitter, mYoke_, "Splitter LV");
  G4LogicalVolume* GapLV =
    new G4LogicalVolume( SplitterGap, mGap_, "Gap LV" );

  G4VPhysicalVolume* splitter = 
    new G4PVPlacement( &rotMtx_, gPos_, elemName_,
		       splitterLV, pMother, false, 0);

  ///////////////////
  // Visualisation //
  ///////////////////
  G4Colour colourSplit(0., 1., 0.); // green
  G4VisAttributes* splitterVisAtt = new G4VisAttributes(true, colourSplit);
  splitterLV-> SetVisAttributes(splitterVisAtt);

//  G4Colour colourSfield(1., 1., 0.); // yellow
  solidField = new G4DisplacedSolid( "Splitter field", fieldBox,
				     0, sField );
//  G4VisAttributes* SfieldVisAtt = new G4VisAttributes(true, colourSfield);
//  splitterLV-> SetVisAttributes(SfieldVisAtt);

  gapLog_ = GapLV;
  yokeLog_ = splitterLV;

  return splitter;
}

////////////////////////////////////////////////////////////
void SplitterMagnet::SetUserLimits( G4UserLimits *limitYoke,
				    G4UserLimits *limitGap )
////////////////////////////////////////////////////////////		    
{
  if( yokeLog_ ) yokeLog_-> SetUserLimits( limitYoke );
  if( gapLog_&&limitGap )
    gapLog_-> SetUserLimits( limitGap );
}
