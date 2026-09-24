/*
  HESDetectorConstruction.cc

  geometry for HES Spectrometer

  D.Kawama
*/

#include "HESDetectorConstruction.hh"
#include "HESGlobalSize.hh"

//=== Sensitive Detector ===//
#include "G4SDManager.hh"
#include "HESTargetSD.hh"
#include "HESVDSD.hh"
#include "EDC1SD.hh"
#include "EDC2SD.hh"
#include "HESEH1SD.hh"
#include "HESEH2SD.hh"

//=== Material Define ===//
#include "MaterialList.hh"

//=== Geometry Define ===//
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trd.hh"
#include "G4SubtractionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4UnionSolid.hh"
#include "G4DisplacedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4VPhysicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4UserLimits.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"
#include "G4AffineTransform.hh"

//=== Beam Line Element ===//
#include "Target.hh"
#include "SplitterMagnet.hh"
#include "Q1Magnet.hh"
#include "Q2Magnet.hh"
#include "DMagnet.hh"
#include "VirtualDetector.hh"
#include "VDetectorPB.hh"
#include "VDetectorQ1.hh"
#include "VDetectorQ2.hh"
#include "EDC1.hh"
#include "EDC2.hh"
#include "EHodo1.hh"
#include "EHodo2.hh"
#include "Collimator.hh"
//#include "TargetChamber.hh"
#include "SieveSlit.hh"

//=== ElectoMagnetic Field ===//
#include "HESField.hh"

#include <sstream>
#include <iomanip>
#include <string>

#include "HESParamMan.hh"
//#include "HESPrimaryGeneratorAction.hh"


//=== Program Start ===//
///////////////////////////////////////////////////
HESDetectorConstruction::HESDetectorConstruction()
  : mList_(0), EMField_(0)
///////////////////////////////////////////////////
{
}

////////////////////////////////////////////////////
HESDetectorConstruction::~HESDetectorConstruction()
///////////////////////////////////////////////////
{
  delete mList_;
  delete EMField_;
}

/////////////////////////////////////////////////////////
MaterialList *HESDetectorConstruction::DefineMaterials()
/////////////////////////////////////////////////////////
{
  MaterialList *ml = new MaterialList;

  return ml;
}

////////////////////////////////////////////////////////
G4VPhysicalVolume *HESDetectorConstruction::Construct()
///////////////////////////////////////////////////////
{
  mList_ = DefineMaterials();

  G4VPhysicalVolume *world = ConstructPayload();

  return world;
}

///////////////////////////////////////////////////////
HESField *HESDetectorConstruction::MakeDetectorField()
///////////////////////////////////////////////////////
{
  return new HESField();
}

///////////////////////////////////////////////////////////////
G4VPhysicalVolume *HESDetectorConstruction::ConstructPayload()
//////////////////////////////////////////////////////////////
{ 
  //Parameters
  HESParamMan *paramMan = HESParamMan::GetParamMan();
  G4double Offset = paramMan->GetOffset()*cm;
  G4double Tilt = paramMan->GetTiltAngle()*degree;
  G4double E_Theta = paramMan->GetRotAngle()*degree;
  G4double FPAng = paramMan->GetFPAngle()*degree;
  G4double Shift = paramMan->GetShift();
  G4double PivAdj = paramMan->GetPivAdj();
  G4RotationMatrix *rotHES = new  G4RotationMatrix();
  rotHES->rotateX(Tilt);
  G4RotationMatrix *rotSplitter = new G4RotationMatrix();
  rotSplitter->rotateY(E_Theta);
  G4SDManager *SDManager = G4SDManager::GetSDMpointer();
  if( !EMField_ ) EMField_ = MakeDetectorField();
  else            EMField_->cleanupElemList();
  
  G4double VTposX = (-9.0648*sin(E_Theta)-24.332*cos(E_Theta)
                     +Shift*sin(E_Theta+8.*degree)
		     +PivAdj)*cm;
  G4double VTposY = Offset;
  G4double VTposZ = (9.0648*cos(E_Theta)-24.332*sin(E_Theta)
                     +Shift*cos(E_Theta+8.*degree)
		     )*cm;
  const G4ThreeVector VTpos(VTposX,VTposY,VTposZ);
  G4AffineTransform *HESTrans = 
    //new G4AffineTransform( rotHES, VTpos );
    new G4AffineTransform( rotHES, VTpos );

  //survey information
  G4ThreeVector dposSPL(0.022*cm,-0.097*cm,0.087*cm);
  G4ThreeVector dposQ1(0.009*cm,0.040*cm,0.074*cm);
  G4ThreeVector dposQ2(0.004*cm,-0.050*cm,0.120*cm);
  G4ThreeVector dposD(-0.015*cm,-0.155*cm,-0.016*cm);
  //Yaw:ccw looking from above
  //Pitch:ccw looking from the beam right
  //Roll:cw looking from upstream
  G4double yawSPL=-0.034*degree;   
  G4double pitchSPL=-0.040*degree; 
  G4double rollSPL=0.012*degree;
  G4double yawQ1=-0.002*degree;
  G4double pitchQ1=-0.059*degree;
  G4double rollQ1=-0.001*degree;
  G4double yawQ2=-0.020*degree;
  G4double pitchQ2=-0.067*degree;
  G4double rollQ2=0.002*degree;
  G4double yawD=0.000*degree;
  G4double pitchD=0.049*degree;
  G4double rollD=-0.013*degree;
  
  //=== World ===//
  G4Box *worldSolid = new G4Box( "World",WorldSizeX,
				 WorldSizeY,WorldSizeZ );

  G4LogicalVolume *worldLV =
    new G4LogicalVolume( worldSolid, mList_->Vacuum, "World LV");

  G4VPhysicalVolume *world = 
    new G4PVPlacement( 0, G4ThreeVector( 0.*cm, 0.*cm, 0.*cm ),
		       worldLV, "World", 0, false, 0);
  worldLV->SetVisAttributes(G4VisAttributes::Invisible);

  //=== Target ===//
  G4double TargetThickness = paramMan->GetTargetThickness();
  std::string TargetMaterial = paramMan->GetTargetMaterial();
  G4Material *Target = mList_->Vacuum;
  
  
  
  if( TargetMaterial == "Vanadium"){
    Target = mList_->V51;
  }
  else if( TargetMaterial == "Yttrium"){
    Target = mList_->Y89;
  }
  else if( TargetMaterial == "Lead"){
    Target = mList_->Pb208;
  }
  else if( TargetMaterial == "Carbon"){
    Target = mList_->C12;
  }
  else if( TargetMaterial == "Silicon"){
    Target = mList_->Si28;
  }
  else if( TargetMaterial == "Polyethylene"){
    Target = mList_->CH2;
  }
  else if( TargetMaterial == "Lithium"){
    Target = mList_->Li7;
  }
  else if( TargetMaterial == "Beryllium"){
    Target = mList_->Be9;
  }
  else if( TargetMaterial == "Boron"){
    Target = mList_->B10;
  }
  else if( TargetMaterial == "Calcium"){
    Target = mList_->Ca40;
  }
  else if( TargetMaterial == "Water"){
    Target = mList_->Water;

  }
  else if( TargetMaterial == "Cromium"){
    Target = mList_->Cr52;
  }
  else {
  G4cout << "You choose strange target '" <<TargetMaterial <<"' !! I don't know such a target..." <<G4endl;
  G4cout << "I decided to use no target!" <<G4endl;
  Target = mList_->Vacuum;
  }
  G4double TargetDensity = Target->GetDensity();
  G4double TargetSizeX = 1.*cm;
  G4double TargetSizeY = 1.*cm;
  G4double TargetSizeZ = 
    (TargetThickness)/(TargetDensity/(g/cm3)*1000)*cm;
  G4RotationMatrix* rotTarget = new G4RotationMatrix();//Rotaion of target
  rotTarget->rotateY(E_Theta - 17.0 * deg);//Rotation of target
  G4Box *targetSolid = new G4Box( "Target",
				  TargetSizeX/2,TargetSizeY/2,TargetSizeZ/2 );
  G4LogicalVolume *targetLV = 
    new G4LogicalVolume( targetSolid, Target, "Target LV");
  
  if (TargetMaterial != "Vacuum"){
    //new G4PVPlacement( rotSplitter, 
    new G4PVPlacement( rotTarget, 
		       G4ThreeVector(0*cm, 0*cm, 0*cm),
		       targetLV, "Target" , worldLV , false, 0);
  }
  //------ Harver Foils ------
  if(TargetMaterial == "Water"){
    //Foil for water target
    G4Material *Foil =mList_->Harver;
    G4double foilz = 0.025 * 0.1 * cm ; // 25 [um]
    //G4double foilz = 0.25 * 0.1 * cm ; // 25 [um]
    G4Box *foilSolid = new G4Box( "foilSolid_box",
				  TargetSizeX/2+2.0/2.0*mm,TargetSizeY/2,foilz/2 );
    G4RotationMatrix* noRot = new G4RotationMatrix();
    G4DisplacedSolid* targetf1 
      = new G4DisplacedSolid("targetf1",
			     foilSolid,
			     noRot ,
			     G4ThreeVector( 0.0 , 0.0 , -TargetSizeZ/2.0 - foilz/2.0 )
			     );
    G4UnionSolid* targetf2 
      = new G4UnionSolid("targetf2",
			 targetf1,
			 foilSolid,
			 noRot,
			 G4ThreeVector( 0.0 , 0.0 , TargetSizeZ/2.0 + foilz/2.0 )
			 );
    G4LogicalVolume *foilLV = 
      new G4LogicalVolume( targetf2, Foil , "Foil LV");
    new G4PVPlacement( rotTarget
		       , G4ThreeVector(0.0*cm, 0.0*cm, 0.0*cm)
		       , foilLV
		       , "Foil" 
		       , worldLV 
		       , false
		       , 0 );
    //=== Visualization for foil of water target ===//
    //G4Colour colourFoil(1.0, 1.0, 1.0); // White
    G4Colour colourFoil(1.0, 0.8, 1.0); //
    G4VisAttributes *foilVisAtt = new G4VisAttributes(true, colourFoil);
    foilLV->SetVisAttributes(foilVisAtt);
  }
  
  
  int flag = paramMan->GetEventFlag();
  int EDCFlag = paramMan->GetEDCFlag();
  int SSFlag = paramMan->GetSSFlag();
  if( flag==0 ){ 
  //=== Splitter Magnet ===//
  G4ThreeVector posSplitter( 0.*cm, 0*cm, 0.*cm );
  posSplitter += dposSPL;
  std::string Splittermap = paramMan->GetSplitterFieldMap();
  G4RotationMatrix *rotSPLmag = new G4RotationMatrix();
  rotSPLmag=rotSplitter;
  rotSPLmag->rotateY(yawSPL); 
  rotSPLmag->rotateX(-pitchSPL); 
  rotSPLmag->rotateZ(-rollSPL); 
  SplitterMagnet *splitter =
    new SplitterMagnet( "Splitter PV", posSplitter, 
                         rotSPLmag, Splittermap );
  splitter->SetMaterials( mList_->Vacuum, mList_->Vacuum );
  splitter->Place( world );
  EMField_->AddElement( splitter );


  std::string Q1map = paramMan->GetQ1FieldMap();
  //=== Quadrupole 1 ===//
  //G4double VTtoQ1 = (224.8)*cm;
  G4double VTtoQ1 = (209.8)*cm;
  //G4double VTtoQ1 = 209.3*cm;
  //G4double VTtoQ1 = 187.9*cm;//for 550MeV Run
  G4ThreeVector preposQ1( 0*cm, 0*cm, VTtoQ1 );
  preposQ1 += dposQ1;
  G4ThreeVector posQ1 = HESTrans->TransformPoint(preposQ1);
  G4RotationMatrix *rotQ1 = new G4RotationMatrix();
  rotQ1=rotHES;
  rotQ1->rotateY(yawQ1); 
  rotQ1->rotateX(-pitchQ1); 
  rotQ1->rotateZ(-rollQ1); 
  Q1Magnet *Q1 = 
  new Q1Magnet( "Q1 Magnet", posQ1, rotQ1, Q1map );
  Q1->SetMaterials( mList_->Fe, mList_->Vacuum );
  Q1->Place( world );
  EMField_->AddElement( Q1 );

  //== Collimator & Sieve slit ==//
//  G4ThreeVector posSS( SSPositionX, SSPositionY, SSPositionZ );
//  G4RotationMatrix *rotSS = new G4RotationMatrix();
//  rotSS->rotateY(E_Theta);
//#ifdef SIEVE_SLIT  
  //G4double VTtoSS = VTtoQ1 - 38.*cm-2.54/2.*cm;
  G4double VTtoSS = VTtoQ1 - 44.783*cm;
  G4ThreeVector preposSS( 0.077*cm, 0.024*cm, VTtoSS );
  G4ThreeVector posSS = HESTrans->TransformPoint(preposSS);
  if (SSFlag==1){
    SieveSlit *Slit = new SieveSlit( "Sieve Slit", posSS, rotHES );
    Slit->SetMaterials( mList_->Heavymet, mList_->Vacuum );
    Slit->Place( world );
  }
//#else
////   rotSS->rotateZ(180.0*degree);
  if (SSFlag==2){
  	Collimator *Coll = new Collimator( "Collimator" , posSS, rotHES);
  	Coll->SetMaterials( mList_->Heavymet, mList_->Vacuum );
  	Coll->Place( world );
  }
//#endif
  //=== Target Chamber (only HKS fringe) ===//
//  G4RotationMatrix *rotTC = new G4RotationMatrix();
// rotTC->rotateY(E_Theta);
//  G4ThreeVector posTC( TCPositionX,
//		       TCPositionY,
//		       TCPositionZ );
//  TargetChamber *targetC =
//    new TargetChamber( "TargetChabmer", posTC, rotTC );
//  targetC->SetMaterials( mList_->Fe, mList_->Vacuum );
//  targetC->Place( world );

  //=== Virtual Detector for Beamprofile ===//
//  G4ThreeVector posVD0( 0.*cm, 0.*cm, 150.*cm );
//  VDetectorPB *VD0 = new VDetectorPB( "VD0", posVD0, 0, 0 );
//  VD0->SetMaterials( mList_->Vacuum );
//  VD0->Place( world );

  //=== Virtual Detector for Q1 Entrance ===//
  //G4double VTtoVD1 = VTtoQ1 - 37.95*cm;
  G4double VTtoVD1 = VTtoQ1-31*cm;
  G4ThreeVector preposVD1( 0.*cm, 0.*cm, VTtoVD1 );
  G4ThreeVector posVD1 = HESTrans->TransformPoint(preposVD1);
  //VDetectorQ1 *VD1 = new VDetectorQ1( "VD1", posVD1, rotHES, 1 );
  G4double VD1SizeX = 300.*cm;
  G4double VD1SizeY = 50.*cm;
  G4double VD1SizeZ = 0.1*cm;
  VirtualDetector *VD1 =
    new VirtualDetector( "VD1", posVD1, rotHES, 
			 VD1SizeX, VD1SizeY, VD1SizeZ, 1 );
  VD1->SetMaterials( mList_->Vacuum );
  VD1->Place( world );

  //=== Virtual Detector for Q1 Exit ===//
  G4double VTtoVD2 = VTtoQ1 + 30*cm;
  G4ThreeVector preposVD2( 0.*cm, 0.*cm, VTtoVD2 );
  G4ThreeVector posVD2 = HESTrans->TransformPoint(preposVD2);
  VDetectorQ1 *VD2 = new VDetectorQ1( "VD2", posVD2, rotHES, 2 );
  VD2->SetMaterials( mList_->Vacuum );
  VD2->Place( world );

  //=== Quadrupole 2 ===//
  G4double VTtoQ2 = VTtoQ1 + 85.0*cm;
  G4ThreeVector preposQ2( 0.*cm, 0.*cm, VTtoQ2 );
  preposQ2 += dposQ2;
  G4ThreeVector posQ2 = HESTrans->TransformPoint(preposQ2);
  std::string Q2map = paramMan->GetQ2FieldMap();
  G4RotationMatrix *rotQ2 = new G4RotationMatrix();
  rotQ2=rotHES;
  rotQ2->rotateY(yawQ2); 
  rotQ2->rotateX(-pitchQ2); 
  rotQ2->rotateZ(-rollQ2); 
  Q2Magnet *Q2 = 
  new Q2Magnet( "Q2 Magnet", posQ2, rotQ2, Q2map );
  Q2->SetMaterials( mList_->Fe, mList_->Vacuum );
  Q2->Place( world );
  EMField_->AddElement( Q2 );

  //=== Virtual Detector for Q2 Entrance ===//
  G4double VTtoVD3 = VTtoQ2 - 25.51*cm;
  G4ThreeVector preposVD3( 0.*cm, 0.*cm, VTtoVD3 );
  G4ThreeVector posVD3 = HESTrans->TransformPoint(preposVD3);
  VDetectorQ2 *VD3 = new VDetectorQ2( "VD3", posVD3, rotHES, 3 );
  VD3->SetMaterials( mList_->Vacuum );
  VD3->Place( world );

  //=== Virtual Detector for Q2 Exit ===//
  G4double VTtoVD4 = VTtoQ2 + 25*cm;
  G4ThreeVector preposVD4( 0.*cm, 0.*cm, VTtoVD4 );
  G4ThreeVector posVD4 = HESTrans->TransformPoint(preposVD4);
  VDetectorQ2 *VD4 = new VDetectorQ2( "VD4", posVD4, rotHES, 4 );
  VD4->SetMaterials( mList_->Vacuum );
  VD4->Place( world );

  //=== Dipole Magnet ==//
  //G4double VTtoDen = VTtoQ2 + 75.0*cm;
  G4double VTtoDen = VTtoQ2 + 105.0*cm;
  G4double DCenter = 220.0*cm;
  G4ThreeVector preDen( 0.*cm, 0.*cm, VTtoDen  );
  G4ThreeVector Den = HESTrans->TransformPoint(preDen);
  G4ThreeVector DCen( DCenter, 0.*cm, 0.*cm  );
  DCen+=dposD;
  G4AffineTransform *DTrans = 
    new G4AffineTransform( 0, DCen );
  G4ThreeVector posD = DTrans->TransformPoint(Den);
  G4RotationMatrix *rotDmag = new  G4RotationMatrix();
  rotDmag->rotateX(Tilt);
  rotDmag->rotateY(155.*degree);
  rotDmag->rotateY(yawD); 
  rotDmag->rotateX(-pitchD); 
  rotDmag->rotateZ(-rollD); 
  std::string Dmap = paramMan->GetDFieldMap();
  DMagnet *Dipole = 
  new DMagnet( "Dipole Magnet", posD, rotDmag ,Dmap );
  Dipole->SetMaterials( mList_->Fe, mList_->Vacuum );
  Dipole->Place( world );
  EMField_->AddElement( Dipole );

  //=== Virtual Detector for Dipole Entrance ===//
  G4double VD5SizeX = 50.*cm;
  G4double VD5SizeY = 17.*cm;
  G4double VD5SizeZ = 0.1*cm;
  G4double posVD5X = Den.x();
  G4double posVD5Y = Den.y();
  G4double posVD5Z = Den.z()-0.51*cm;
  G4ThreeVector posVD5(posVD5X,posVD5Y,posVD5Z);
  VirtualDetector *VD5 =
    new VirtualDetector( "VD5", posVD5, rotHES, 
			 VD5SizeX, VD5SizeY, VD5SizeZ, 5 );
  VD5->SetMaterials( mList_->Vacuum );
  VD5->Place( world );

  //=== Virtual Detector for Dipole exit ===//
  G4double VD6SizeX = 50.*cm;
  G4double VD6SizeY = 17.*cm;
  G4double VD6SizeZ = 0.001*cm;
  G4double VD5toVD6 = 186.0*cm;
  G4ThreeVector VD5to6(0.*cm,0.*cm,VD5toVD6);
  //G4AffineTransform *OnlyTilt = new G4AffineTransform( rotHES,0 );
  G4AffineTransform *OnlyTilt = new G4AffineTransform( rotHES , G4ThreeVector(0.0,0.0,0.0) );
  G4ThreeVector preposVD6 = OnlyTilt->TransformPoint(VD5to6);
  G4ThreeVector DenTOnly = OnlyTilt->TransformPoint(preDen);
  G4ThreeVector Tiltax(0.*cm,cos(Tilt)*cm,-sin(Tilt)*cm);
  G4RotationMatrix *rotD = 
  new G4RotationMatrix(Tiltax, -25.*degree);
  G4AffineTransform *DRot = new G4AffineTransform( rotD,Den );
  G4ThreeVector posVD6 = DRot->TransformPoint(preposVD6);
  //G4RotationMatrix *rotVD = new G4RotationMatrix(Tiltax,-(50.-0.6)*degree);
  //G4RotationMatrix *rotVD = new G4RotationMatrix(Tiltax,-(50.-0.3)*degree);
  G4RotationMatrix *rotVD = new G4RotationMatrix(Tiltax,-(50.-0.1)*degree);
  rotVD->rotateX(Tilt);
  rotVD->rotateX(-1.7*degree);
  //rotVD->rotateX(-1.8*degree);
  /*G4RotationMatrix *rotVD = new G4RotationMatrix();
  rotVD->rotateX(Tilt);
  rotVD->rotateY(-50.*degree);*/
  //rotVD->rotate(-50.*degree,Tiltax);
  //G4RotationMatrix *rotVD = new G4RotationMatrix();
  //rotVD->rotateY(-50.*degree);
  //rotVD->rotateX(Tilt);
  VirtualDetector *VD6 =
    new VirtualDetector( "VD6", posVD6, rotVD,
			 VD6SizeX, VD6SizeY, VD6SizeZ, 6 );
  VD6->SetMaterials( mList_->Vacuum );
  VD6->Place( world );

  G4RotationMatrix *rotDet = 
  new G4RotationMatrix(Tiltax, -50.*degree);
  //G4AffineTransform *DetTrans = new G4AffineTransform( rotDet,0 );
  const G4ThreeVector zero (0.0,0.0,0.0);
  G4AffineTransform *DetTrans = new G4AffineTransform( rotDet , zero );
  G4double VDSizeX = 300.*cm;
  G4double VDSizeY = 100.*cm;
  G4double VDSizeZ = 0.001*cm;
  
  //=== ED Exit ===//
  if (EDCFlag!=0){
  G4double VD6toW1z = 107.*cm;
  G4ThreeVector VD6toW1( 0.*cm, 0.*cm,  VD6toW1z);
  G4ThreeVector W1TOnly = OnlyTilt->TransformPoint(VD6toW1);
  G4ThreeVector preposW1 = DetTrans->TransformPoint(W1TOnly);
  G4ThreeVector posW1 = preposW1+posVD6;
  G4Box *W1Solid = new G4Box( "Window1",105./2.*cm,17.5/2.*cm,0.02/2.*cm );
  G4LogicalVolume *W1LV = 
     new G4LogicalVolume( W1Solid, mList_->Kevlar, "Window1LV");
     //new G4LogicalVolume( W1Solid, mList_->Vacuum, "Window1LV");
   new G4PVPlacement( rotVD, posW1,
		    W1LV, "Window1" , worldLV , false, 0);
  G4double VD6toW2z = (107.+0.02+0.01/2.)*cm;
  G4ThreeVector VD6toW2( 0.*cm, 0.*cm,  VD6toW2z);
  G4ThreeVector W2TOnly = OnlyTilt->TransformPoint(VD6toW2);
  G4ThreeVector preposW2 = DetTrans->TransformPoint(W2TOnly);
  G4ThreeVector posW2 = preposW2+posVD6;
  G4Box *W2Solid = new G4Box( "Window2",105./2.*cm,17.5/2.*cm,0.01/2.*cm );
  G4LogicalVolume *W2LV = 
     new G4LogicalVolume( W2Solid, mList_->Mylar, "Window2LV");
     //new G4LogicalVolume( W2Solid, mList_->Vacuum, "Window2LV");
   new G4PVPlacement( rotVD, posW2,
		    W2LV, "Window2" , worldLV , false, 0);
  }
  
  //=== Drift Chamber ===//
  //G4double VD6toEDC1z = (53.+16.)*cm;
  G4double VD6toEDC1z = 135.*cm;
  G4ThreeVector VD6toEDC1( 0.*cm, -1.*cm,  VD6toEDC1z);
  G4ThreeVector EDC1TOnly = OnlyTilt->TransformPoint(VD6toEDC1);
  G4ThreeVector preposEDC1 = DetTrans->TransformPoint(EDC1TOnly);
  G4ThreeVector posEDC1 = preposEDC1+posVD6;
  EDC1 *DC1 =
    new EDC1( "EDC1", posEDC1, rotVD, 0 );
  DC1->SetMaterials( mList_->Vacuum, mList_->DCGas,
                     mList_->Mylar, mList_->DCGas, 
                     mList_->W, mList_->Cu, mList_->Cu );
  //material is Box, Gas, Mylar, Layer
  //DC1->SetMaterials( mList_->Vacuum, mList_->Vacuum,
  //                   mList_->Vacuum, mList_->Vacuum );
  if (EDCFlag!=0){
    DC1->Place( world );
  }
  
  if (EDCFlag!=0){
  G4double VD6toAirz = 154.85*cm;
  G4ThreeVector VD6toAir( 0.*cm, 0.*cm,  VD6toAirz);
  G4ThreeVector AirTOnly = OnlyTilt->TransformPoint(VD6toAir);
  G4ThreeVector preposAir = DetTrans->TransformPoint(AirTOnly);
  G4ThreeVector posAir = preposAir+posVD6;
  G4Box *AirSolid = new G4Box( "Air",100.*cm,50.*cm,7.7/2*cm );
  G4LogicalVolume *AirLV = 
     new G4LogicalVolume( AirSolid, mList_->Air, "AirLV");
     //new G4LogicalVolume( AirSolid, mList_->Vacuum, "AirLV");
   new G4PVPlacement( rotVD, posAir,
		    AirLV, "Air" , worldLV , false, 0);
  }

  //G4double VD6toEDC2z = 161.2*cm;
  G4double VD6toEDC2z = 166.0*cm;
  G4ThreeVector VD6toEDC2( 0.*cm, -1.*cm,  VD6toEDC2z);
  G4ThreeVector EDC2TOnly = OnlyTilt->TransformPoint(VD6toEDC2);
  G4ThreeVector preposEDC2 = DetTrans->TransformPoint(EDC2TOnly);
  G4ThreeVector posEDC2=preposEDC2+posVD6;
  EDC2 *DC2 =
    new EDC2( "EDC2", posEDC2, rotVD, 0 );
  DC2->SetMaterials( mList_->Vacuum, mList_->DCGas,
                     mList_->Mylar, mList_->DCGas,
                     mList_->W, mList_->W);
  //material is Box, Gas, Mylar, Layer
  //DC2->SetMaterials( mList_->Vacuum, mList_->Vacuum,
  //                   mList_->Vacuum, mList_->Vacuum );
  if (EDCFlag!=0){
    DC2->Place( world );
  }
  
  //=== Virtual Detectors for Detector Plane ===//
  G4double VD6toVD7 = 45.5*cm;
  G4ThreeVector VD6to7( 0.*cm, -1.*cm,  VD6toVD7);
  G4ThreeVector VD7TOnly = OnlyTilt->TransformPoint(VD6to7);
  G4ThreeVector preposVD7 = DetTrans->TransformPoint(VD7TOnly);
  G4ThreeVector posVD7=preposVD7+posVD6;
  VirtualDetector *VD7 =
    new VirtualDetector( "VD7", posVD7, rotVD,
			 VDSizeX, VDSizeY, VDSizeZ, 7 );
  VD7->SetMaterials( mList_->Vacuum );
  VD7->Place( world );
  
  
  //===//

  G4double VD6toVD8 = 120.*cm;
  G4ThreeVector VD6to8( 0.*cm, -1.*cm,  VD6toVD8);
  G4ThreeVector VD8TOnly = OnlyTilt->TransformPoint(VD6to8);
  G4ThreeVector preposVD8 = DetTrans->TransformPoint(VD8TOnly);
  G4ThreeVector posVD8=preposVD8+posVD6;
  VirtualDetector *VD8 =
    new VirtualDetector( "VD8", posVD8, rotVD,
			 VDSizeX, VDSizeY, VDSizeZ, 8 );
  VD8->SetMaterials( mList_->Vacuum );
  VD8->Place( world );
  
  //===//

  G4double VD6toVD9 = 135.*cm;
  G4ThreeVector VD6to9( 0.*cm, -1.*cm,  VD6toVD9);
  G4ThreeVector VD9TOnly = OnlyTilt->TransformPoint(VD6to9);
  G4ThreeVector preposVD9 = DetTrans->TransformPoint(VD9TOnly);
  G4ThreeVector posVD9=preposVD9+posVD6;
  /*G4RotationMatrix *rotFP =  
  new G4RotationMatrix(Tiltax, -50.*degree-FPAng);
  rotFP->rotateX(Tilt);*/
  VirtualDetector *VD9 =
    new VirtualDetector( "VD9", posVD9, rotVD,
			 VDSizeX, VDSizeY, VDSizeZ, 9 );
  VD9->SetMaterials( mList_->Vacuum );
  VD9->Place( world );
  
  //===//

  G4double VD6toVD10 = 155.*cm;
  G4ThreeVector VD6to10( 0.*cm, 0.*cm,  VD6toVD10);
  G4ThreeVector VD10TOnly = OnlyTilt->TransformPoint(VD6to10);
  G4ThreeVector preposVD10 = DetTrans->TransformPoint(VD10TOnly);
  G4ThreeVector posVD10=preposVD10+posVD6;
  VirtualDetector *VD10 =
    new VirtualDetector( "VD10", posVD10, rotVD,
			 VDSizeX, VDSizeY, VDSizeZ, 10 );
  VD10->SetMaterials( mList_->Vacuum );
  VD10->Place( world );
  
  //===//

  //G4double VD6toVD11 = 161.2*cm;
  G4double VD6toVD11 = 180*cm;
  G4ThreeVector VD6to11( 0.*cm, 0.*cm,  VD6toVD11);
  G4ThreeVector VD11TOnly = OnlyTilt->TransformPoint(VD6to11);
  G4ThreeVector preposVD11 = DetTrans->TransformPoint(VD11TOnly);
  G4ThreeVector posVD11=preposVD11+posVD6;
  VirtualDetector *VD11 =
    new VirtualDetector( "VD11", posVD11, rotVD,
			 VDSizeX, VDSizeY, VDSizeZ, 11 );
  VD11->SetMaterials( mList_->Vacuum );
  VD11->Place( world );
  
  
  G4double VD12SizeX = 100.*cm;
  G4double VD12SizeY = 100.*cm;
  G4double VD12SizeZ = 0.1*cm;
  //G4double VTtoVD12 = VTtoQ1-30.*cm-2.55*cm;
  //G4ThreeVector preposVD12( 0*cm, 0*cm, VTtoVD12 );
  //G4ThreeVector posVD12 = HESTrans->TransformPoint(preposVD12);
  G4ThreeVector posVD12 = posSS;
  VirtualDetector *VD12 =
    new VirtualDetector( "VD12", posVD12, rotHES,
			 VD12SizeX, VD12SizeY, VD12SizeZ, 12 );
  VD12->SetMaterials( mList_->Vacuum );
  VD12->Place( world );
  
  /*G4double VD13SizeX = 100.*cm;
  G4double VD13SizeY = 100.*cm;
  G4double VD13SizeZ = 0.1*cm;
  G4double VTtoVD13 = VTtoQ1-60.*cm;
  G4ThreeVector preposVD13( 0*cm, 0*cm, VTtoVD13 );
  G4ThreeVector posVD13 = HESTrans->TransformPoint(preposVD13);
  VirtualDetector *VD13 =
    new VirtualDetector( "VD13", posVD13, rotHES,
			 VD13SizeX, VD13SizeY, VD13SizeZ, 13 );
  VD13->SetMaterials( mList_->Vacuum );
  VD13->Place( world );*/
  G4double VD13SizeX = 150.*cm;
  G4double VD13SizeY = 50.*cm;
  G4double VD13SizeZ = 0.5*cm;
  G4double VTtoVD13 = 150.0*cm;
  G4ThreeVector preposVD13( 0*cm, -Offset, VTtoVD13 );
  G4RotationMatrix *rotVD13 = new  G4RotationMatrix();
  rotVD13->rotateY(4.8*degree);
  G4AffineTransform *VD13Trans = 
    new G4AffineTransform( 0, VTpos );
  G4ThreeVector posVD13 = VD13Trans->TransformPoint(preposVD13);
  VirtualDetector *VD13 =
    new VirtualDetector( "VD13", posVD13, rotVD13,
			 VD13SizeX, VD13SizeY, VD13SizeZ, 13 );
  VD13->SetMaterials( mList_->Vacuum );
  VD13->Place( world );
  
  G4double VD14SizeX = 200.*cm;
  G4double VD14SizeY = 30.*cm;
  G4double VD14SizeZ = 0.5*cm;
  G4double VTtoVD14 = VTtoQ1-124.6*cm;
  G4ThreeVector preposVD14( 0*cm, -Offset, VTtoVD14 );
  G4RotationMatrix *rotVD14 = new  G4RotationMatrix();
  rotVD14->rotateY(E_Theta-82.*degree);
  G4AffineTransform *VD14Trans = 
    new G4AffineTransform( 0, VTpos );
  G4ThreeVector posVD14 = VD14Trans->TransformPoint(preposVD14);
  VirtualDetector *VD14 =
    new VirtualDetector( "VD14", posVD14, rotVD14,
			 VD14SizeX, VD14SizeY, VD14SizeZ, 14 );
  VD14->SetMaterials( mList_->Vacuum );
  VD14->Place( world );
  //=== HES HodoScope ===//
  //=== Ehodo 1 ===//
  
  //G4double VD6toEH1 = 180.1*cm;
  G4double VD6toEH1 = 178.2*cm;
  G4ThreeVector VD6toeh1( 0.*cm, 0.*cm,  VD6toEH1);
  G4ThreeVector EH1TOnly = OnlyTilt->TransformPoint(VD6toeh1);
  G4ThreeVector preposEH1 = DetTrans->TransformPoint(EH1TOnly);
  G4ThreeVector posEH1=preposEH1+posVD6;
  EHodo1 *ehodo1 = 
    new EHodo1( "EHodo 1", posEH1, rotVD, 0 );
  ehodo1->SetMaterial( mList_->Scinti );
 
  //===//
  //=== Ehodo 2 ===//

  //G4double VD6toEH2 = 210.1*cm;
  G4double VD6toEH2 = 208.2*cm;
  G4ThreeVector VD6toeh2( 0.*cm, 0.*cm,  VD6toEH2);
  G4ThreeVector EH2TOnly = OnlyTilt->TransformPoint(VD6toeh2);
  G4ThreeVector preposEH2 = DetTrans->TransformPoint(EH2TOnly);
  G4ThreeVector posEH2=preposEH2+posVD6;
  EHodo2 *ehodo2 = 
    new EHodo2( "EHodo 2", posEH2, rotVD, 0 );
  ehodo2->SetMaterial( mList_->Scinti );
  
  ehodo1->Place(world);
  EMField_->AddElement(ehodo1);
  ehodo2->Place(world);
  EMField_->AddElement(ehodo2);
 
  /////////////////////////
  // Sensitive Detectors //
  /////////////////////////
  
  //=== Virtual Detector ===//
  HESVDSD *VDSD = new HESVDSD( "/HES/VD" );
  SDManager->AddNewDetector( VDSD );
  VD1->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD2->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD3->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD4->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD5->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD6->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD7->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD8->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD9->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD10->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD11->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD12->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD13->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VD14->GetDetectorLV()->SetSensitiveDetector( VDSD );
  VDSD->SetElement( 1, VD1 );
  VDSD->SetElement( 2, VD2 );
  VDSD->SetElement( 3, VD3 );
  VDSD->SetElement( 4, VD4 );
  VDSD->SetElement( 5, VD5 );
  VDSD->SetElement( 6, VD6 );
  VDSD->SetElement( 7, VD7 );
  VDSD->SetElement( 8, VD8 );
  VDSD->SetElement( 9, VD9 );
  VDSD->SetElement( 10, VD10 );
  VDSD->SetElement( 11, VD11 );
  VDSD->SetElement( 12, VD12 );
  VDSD->SetElement( 13, VD13 );
  VDSD->SetElement( 14, VD14 );

  //=== Drift Chamber ===//
  if (EDCFlag!=0){
  	EDC1SD *DC1SD = new EDC1SD( "/HES/DC1" );
  	SDManager->AddNewDetector( DC1SD );
  	DC1->GetDetectorLV()->SetSensitiveDetector( DC1SD );
  	DC1SD->SetElement( DC1 );
  	EDC2SD *DC2SD = new EDC2SD( "/HES/DC2" );
  	SDManager->AddNewDetector( DC2SD );
  	DC2->GetDetectorLV()->SetSensitiveDetector( DC2SD );
  	DC2SD->SetElement( DC2 );
  
  }
  	//=== HES HodoScope ===//
  	EH1SD *eh1SD = new EH1SD( "/HES/EH1" );
  	SDManager->AddNewDetector( eh1SD );
  	ehodo1->GetDetectorLV()->SetSensitiveDetector( eh1SD );
  	eh1SD->SetElement( ehodo1 );
  	EH2SD *eh2SD = new EH2SD( "/HES/EH2" );
  	SDManager->AddNewDetector( eh2SD );
  	ehodo2->GetDetectorLV()->SetSensitiveDetector( eh2SD );
  	eh2SD->SetElement( ehodo2 );

#ifdef SIEVE_SLIT
  //=== Sieve Slit ===//
//  HESSSSD *SSSD = new HESSSSD( "/HES/SS" );
//  SDManager->AddNewDetector( SSSD );
//  Slit->SetSD( SSSD );
//  SSSD->SetElement( Slit );
#else
//  VDSD->SetElement( 7, Coll );
//  Coll->GetDetectorLV()->SetSensitiveDetector( VDSD );
#endif
  } //if(Event Flag)
  
  //=== Target ==//
    HESTargetSD *targetSD = new HESTargetSD( "/HES/Target/" );
    SDManager->AddNewDetector( targetSD );
    targetLV->SetSensitiveDetector( targetSD );

  ///////////////////
  // Visualisation //
  ///////////////////

  //=== World ===//
  worldLV->SetVisAttributes(G4VisAttributes::Invisible);

  //=== Splitter Yoke ===//

  //=== Q1 Yoke ===//

  //=== Q2 Yoke ===//

  //=== Target ===//
    G4Colour colourTarget(0., 1., 1.); // cyan
    G4VisAttributes *targetVisAtt = new G4VisAttributes(true, colourTarget);
    targetLV->SetVisAttributes(targetVisAtt);


  ///////////////////////
  // Special User Cuts //
  ///////////////////////
//  G4UserLimits  *userLimitsYoke =
//    new G4UserLimits( 5.*cm, 30.*cm, 100.*ns, 10.*MeV, 5.*cm);
//  splitter->SetUserLimits( userLimitsYoke );
//  Q1->SetUserLimits( userLimitsYoke );
//  Q2->SetUserLimits( userLimitsYoke );
//  Dipole->SetUserLimits( userLimitsYoke );
//  CExtension->SetUserLimits( userLimitsYoke );
//  targetC->SetUserLimits( userLimitsYoke );

  return world;
}

