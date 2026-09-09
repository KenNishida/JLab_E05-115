/*
  Q2Magnet.cc

  M.Kawai
*/

#include "Q2Magnet.hh"
#include "Q2FieldMan.hh"

#include "G4Box.hh"
#include "G4Trd.hh"
#include "G4Tubs.hh"
#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4DisplacedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "HKSParamMan.hh"
#include <cmath>

//const std::string deffilename = "param/HKS_Q2_test.map";

/////////////////////////////////////////////////////
Q2Magnet::Q2Magnet( const G4String &name,
		    const G4ThreeVector &gPos,
		    const G4RotationMatrix *rotMtx,
		    const std::string deffilename)
  : BeamLineElement( name, gPos, rotMtx ),
    solidField(0), yokeLog_(0),gapLog_(0), 
    fMan(deffilename)
/////////////////////////////////////////////////////
{
  fMan.Initialize();
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  MapID = paramMan->GetQ2MapID();
  Scale = paramMan->GetQ2ScaleFactor();
}

/////////////////////
Q2Magnet::~Q2Magnet()
/////////////////////
{
}

////////////////////////////////////////////////
bool Q2Magnet::ExistMagneticField( void ) const
////////////////////////////////////////////////
{
  return true;
}

///////////////////////////////////////////////////////////
bool Q2Magnet::IsInside( const G4ThreeVector &gPos ) const
///////////////////////////////////////////////////////////
{
  G4ThreeVector lPos = GetLocalPosition(gPos);
//  if( solidField->Inside(lPos)!=kOutside ){
    if( lPos.x()>-50.*cm && lPos.x()<50.*cm &&
        lPos.y()>-50.*cm && lPos.y()<50.*cm &&
        lPos.z()>-50.*cm && lPos.z()<50.*cm 
    )
  {  
    return true;
  }
  else return false;
}

////////////////////////////////////////////////////
G4ThreeVector Q2Magnet::
GetMagneticField( const G4ThreeVector &gPos ) const
////////////////////////////////////////////////////
{
  if( IsInside(gPos) ){
    G4ThreeVector lPos = GetLocalPosition( gPos );
    return GetGlobalDirection( Field(lPos) );
    //return GetGlobalDirection( KatoEq(lPos) );
  }
  else return G4ThreeVector();
}


/////////////////////////////////////////////////////////////////
G4ThreeVector Q2Magnet::KatoEq( const G4ThreeVector &pos ) const
/////////////////////////////////////////////////////////////////
{
  G4double G = 29.*cm; // Q2 Bore diameter
  G4double x, y, z;
  x = pos.x(); y = pos.y(); z = pos.z();
  G4double Q2L = 60.*cm; // Q2 Length
  G4double s = (fabs(z)-Q2L/2)/G;
  G4double par[4];
  par[0] = -0.9842;
  par[1] = 6.3375;
  par[2] = -3.5134;
  par[3] = 0.9895;
  G4double ds[3];
  ds[0] = par[0] + par[1]*s +   par[2]*s*s +   par[3]*s*s*s;
  ds[1] =        + par[1]   + 2*par[2]*s   + 3*par[3]*s*s;
  ds[2] =                     2*par[2]     + 6*par[3]*s;
  G4double h = 1/(1+exp(ds[0]));
  G4double dh[2];
  dh[0] = -ds[1]*exp(ds[0])*h*h;
  dh[1] = 
    (-ds[2]-ds[1]*ds[1])*exp(ds[0])*h*h+2*pow((ds[1]*exp(ds[0])),2)*h*h*h;

  G4double Bq1 = 0.3405*1000*Scale*gauss/cm; // Q2 Field Gradient
  G4double Bfield[3];
  for(int i=0; i<3; i++) Bfield[i]=0;
  //  Bfield[0] = Bq1*( y*h -y*(y*y+3*x*y)/(12*G*G)*dh[1]);
  Bfield[0] = Bq1*( y*h -y*(y*y+3*x*x)/(12*G*G)*dh[1]);
  Bfield[1] = Bq1*( x*h -x*(x*x+3*y*y)/(12*G*G)*dh[1]);
  Bfield[2] = Bq1*x*y*dh[0]/G;
  G4ThreeVector B( Bfield[0], Bfield[1], Bfield[2] );

  return B;

}

////////////////////////////////////////////////////////////////
G4ThreeVector Q2Magnet::Field( const G4ThreeVector &pos ) const
////////////////////////////////////////////////////////////////
{

  double X[3];
  double Bfield[3];
 

  X[0] = pos.x()/cm;
  X[1] = pos.y()/cm;
  X[2] = pos.z()/cm;

  if( fMan.GetFieldValue(X, Bfield) ){
    //Bfield[0] *= -1*Scale*gauss;
    //Bfield[1] *= -1*Scale*gauss;
    //Bfield[2] *= -1*Scale*gauss;
    Bfield[0] *= Scale*gauss;
    Bfield[1] *= Scale*gauss;
    Bfield[2] *= Scale*gauss;
  }
  else{
    Bfield[0] = Bfield[1] = Bfield[2] = 0;
  }
  G4ThreeVector B( Bfield[0], Bfield[1], Bfield[2] );

  return B;
}

////////////////////////////////////////////////////////////////
G4VPhysicalVolume *Q2Magnet::Place( G4VPhysicalVolume *pMother )
////////////////////////////////////////////////////////////////
{
  G4ThreeVector dxyz_Q2Yoke( 110.*cm, 190.*cm, 60.*cm );
  G4Box* Q2box = new G4Box( "Q2 box",
			    dxyz_Q2Yoke.x()/2,
			    dxyz_Q2Yoke.y()/2,
			    dxyz_Q2Yoke.z()/2 );

  G4double Width = 61.*cm;
  G4double Radius = 29.*cm;
  G4double Trdy1 = 20.4*cm;
  G4double Trdy2 = 3.8*cm;
  G4double Trdx  = 24.6*cm;
  
  G4Tubs *Tub = new G4Tubs("Q2Tub", 0.*cm, Radius/2., Width/2.,
                          0.*degree, 360.*degree);
  G4Trd  *Trd = new G4Trd("Q2Trd", 
                          Width/2., Width/2.,
                          Trdy1/2., Trdy2/2., 
			  Trdx/2.);
  G4RotationMatrix *rotTrdL = new G4RotationMatrix();
  rotTrdL-> rotateY(-90.*degree);
  G4RotationMatrix *rotTrdR = new G4RotationMatrix();
  rotTrdR-> rotateY(90.*degree);
  G4DisplacedSolid *TrdL = new G4DisplacedSolid(
                           "Q2 TrdL", Trd, rotTrdL,
			   G4ThreeVector((10.3*cm+Trdx/2.),0.,0.));
  G4DisplacedSolid *TrdR = new G4DisplacedSolid(
                           "Q2 TrdR", Trd, rotTrdR,
			   G4ThreeVector(-(10.3*cm+Trdx/2.),0.,0.));

  G4SubtractionSolid* Q2Yoke1 =
    new G4SubtractionSolid( "Q2 Gap1", Q2box, Tub );
  G4SubtractionSolid* Q2Yoke2 =
    new G4SubtractionSolid( "Q2 Gap2", Q2Yoke1, TrdR );
  G4SubtractionSolid* Q2Yoke3 =
    new G4SubtractionSolid( "Q2 Gap3", Q2Yoke2, TrdL );

  G4LogicalVolume* Q2YokeLV =
    new G4LogicalVolume( Q2Yoke3, mYoke_, "Q2Yoke LV");
  //G4LogicalVolume *Q2GapLV =
  //  new G4LogicalVolume( Q2GapSolid, mGap_, "Q2 Gap LV" );

  G4VPhysicalVolume *Q2Yoke = 
    new G4PVPlacement( &rotMtx_, gPos_, "Q2",
		       Q2YokeLV, pMother, false, 0);
  //G4VPhysicalVolume *Q2Gap = 
  //G4PVPlacement( &rotMtx_, gPos_, elemName_+" Gap",
	//	       Q2GapLV, pMother, false, 0);


  ///////////////////
  // Visualisation //
  ///////////////////
  G4Colour colourQ2Yoke(1., 0., 1.); // magenta
  G4VisAttributes* Q2YokeVisAtt = new G4VisAttributes(true, colourQ2Yoke);
  Q2YokeLV-> SetVisAttributes(Q2YokeVisAtt);
  //Q2GapLV-> SetVisAttributes(G4VisAttributes::Invisible);
  //Q2YokeLV-> SetVisAttributes(G4VisAttributes::Invisible);
  //Q2GapLV-> SetVisAttributes(Q2YokeVisAtt);

  solidField = new G4Box( "Q2 field", 50.*cm, 50.*cm, 50.*cm );


  //gapLog_ = Q2GapLV;
  yokeLog_ = Q2YokeLV;

  delete rotTrdR;
  delete rotTrdL;
  //delete Q2YokeVisAtt;
  return Q2Yoke;
}



//////////////////////////////////////////////////////
void Q2Magnet::SetUserLimits( G4UserLimits *limitYoke,
			      G4UserLimits *limitGap )
//////////////////////////////////////////////////////			      
{
  if( yokeLog_ ) yokeLog_-> SetUserLimits(limitYoke);
  if( gapLog_ && limitGap )
    gapLog_-> SetUserLimits(limitGap);
}
