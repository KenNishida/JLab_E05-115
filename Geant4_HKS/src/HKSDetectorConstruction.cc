/*
  HKSDetectorConstruction.cc

  geometry for HKS Spectrometer

  M.Kawai
*/

#include "HKSDetectorConstruction.hh"
#include "HKSGlobalSize.hh"
//#include "HKSGeometryParameter.hh"
#include "DetectorHut.hh"
//=== Sensitive Detector ===//
#include "G4SDManager.hh"
#include "HKSTargetSD.hh"
#include "HKSVDSD.hh"
#include "HKSTOFSD.hh"
#include "HKSACSD.hh"
#include "HKSWCSD.hh"
#include "HKSDCSD.hh"
#include "HKSSSSD.hh"

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
#include "Collimator.hh"
#include "TargetChamber.hh"
#include "SieveSlit.hh"
#include "WaterC.hh"
#include "ChamberExtension.hh"
#include "ChamberExtension2.hh"
#include "HTOF1X.hh"
#include "HTOF1Y.hh"
#include "HTOF2X.hh"
#include "DriftChamber.hh"
#include "AerogelC.hh"

// === === //
#include "NMRPort.hh" //NMR Ports
#include "CEF.hh"     //Chamber Extention Flange

//=== ElectoMagnetic Field ===//
#include "HKSField.hh"

#include <sstream>
#include <iomanip>
#include <string>

#include "HKSParamMan.hh"
//#include "HKSPrimaryGeneratorAction.hh"

//=== Program Start ===//
///////////////////////////////////////////////////
HKSDetectorConstruction::HKSDetectorConstruction()
  : mList_(0), EMField_(0)
///////////////////////////////////////////////////
{
}

////////////////////////////////////////////////////
HKSDetectorConstruction::~HKSDetectorConstruction()
///////////////////////////////////////////////////
{
  delete mList_;
  delete EMField_;
}

/////////////////////////////////////////////////////////
MaterialList *HKSDetectorConstruction::DefineMaterials()
/////////////////////////////////////////////////////////
{
  MaterialList *ml = new MaterialList;

  return ml;
}

////////////////////////////////////////////////////////
G4VPhysicalVolume *HKSDetectorConstruction::Construct()
///////////////////////////////////////////////////////
{
  mList_ = DefineMaterials();

  G4VPhysicalVolume *world = ConstructPayload();

  return world;
}

///////////////////////////////////////////////////////
HKSField *HKSDetectorConstruction::MakeDetectorField()
///////////////////////////////////////////////////////
{
  return new HKSField();
}

///////////////////////////////////////////////////////////////
G4VPhysicalVolume *HKSDetectorConstruction::ConstructPayload()
//////////////////////////////////////////////////////////////
{ 
    //Parameters
  G4double Theta = 32.57*degree;
  //G4double Theta = 33.25*degree;
  G4double preVT = 25.557*cm  ;
  //G4double preVT = 23.557*cm  ;
  //G4double preVT = 24.55*cm  ;
  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  G4RotationMatrix *rotHKS = new  G4RotationMatrix();
    rotHKS->rotateY(Theta);
  G4RotationMatrix *rotHKSSS = new  G4RotationMatrix();
    rotHKSSS->rotateY(Theta-0*degree);
  G4SDManager *SDManager = G4SDManager::GetSDMpointer();
  
  const G4ThreeVector VTpos(preVT*cos(Theta),0.*cm,preVT*sin(Theta));
  //G4ThreeVector VTpos(preVT*cos(Theta)-4.64*cm,0.*cm,preVT*sin(Theta)-2.96*cm);
  G4AffineTransform *HKSTrans = 
    new G4AffineTransform( rotHKS, VTpos );
  
  
  
  if( !EMField_ ) EMField_ = MakeDetectorField();
  else            EMField_->cleanupElemList();
 
  //Survey data
  G4ThreeVector dposSPL(0.022*cm,-0.097*cm, 0.087*cm);
  G4ThreeVector dposQ1(-0.022*cm,-0.119*cm,-0.058*cm);
  G4ThreeVector dposQ2(-0.063*cm,-0.086*cm,-0.077*cm);
  G4ThreeVector dposD(-0.057*cm,-0.035*cm, 0.011*cm);
  //Yaw:ccw looking from above
  //Pitch:ccw looking from the beam right
  //Roll:cw looking from upstream
  G4double yawSPL=-0.034*degree;   
  G4double pitchSPL=-0.040*degree; 
  G4double rollSPL=0.012*degree;
  G4double yawQ1=-0.017*degree;
  G4double pitchQ1=0.025*degree;
  G4double rollQ1=0.012*degree;
  G4double yawQ2=-0.003*degree;
  G4double pitchQ2=0.021*degree;
  G4double rollQ2=0.018*degree;
  G4double yawD=0.006*degree;
  G4double pitchD=-0.001*degree;
  G4double rollD=0.008*degree;
   
  
  //=== World ===//
  G4Box *worldSolid = new G4Box( "World_box",WorldSizeX,
				 WorldSizeY,WorldSizeZ );

  G4LogicalVolume *worldLV =
    new G4LogicalVolume( worldSolid, mList_->Vacuum, "World LV");

  G4VPhysicalVolume *world = 
    new G4PVPlacement( 0, G4ThreeVector( 0.*m, 0.*m, 0.*m ),
		       worldLV, "World", 0, false, 0);
  worldLV->SetVisAttributes(G4VisAttributes::Invisible);

  //=== Target ===//
  G4double TargetThickness = paramMan->GetTargetThickness();
  std::string TargetMaterial = paramMan->GetTargetMaterial();
  G4Material *Target =mList_->Vacuum;
  
  
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
  G4cout << "You choose strange target '" <<TargetMaterial <<"' !! I don't know such a target...orz" <<G4endl;
  G4cout << "I decided to use no target!" <<G4endl;
  Target = mList_->Vacuum;
  }
  G4double TargetDensity = Target->GetDensity();
  G4double TargetSizeX = 1.*cm;
  G4double TargetSizeY = 1.*cm;
  G4double TargetSizeZ = (TargetThickness)/(TargetDensity/(g/cm3)*1000)*cm;
  G4Box *targetSolid = new G4Box( "Target",
				  TargetSizeX/2,TargetSizeY/2,TargetSizeZ/2 );
  G4LogicalVolume *targetLV = 
    new G4LogicalVolume( targetSolid, Target, "Target LV");
  G4RotationMatrix* rotTarget = new G4RotationMatrix();//Rotaion of target
  rotTarget->rotateY(-17.0 * deg);//Rotation of target
  
  if (TargetMaterial != "Vacuum"){
    //new G4PVPlacement( 0, G4ThreeVector(0*cm, 0*cm, 0*cm),
    new G4PVPlacement( rotTarget, G4ThreeVector(0*cm, 0*cm, 0*cm),
		       targetLV, "Target" , worldLV , false, 0);
  }
  //----- Harver Foil --------
  if(TargetMaterial == "Water"){
    G4Material *Foil =mList_->Harver;
    G4double foilz = 0.025 * 0.1 * cm ; // 25 [um]
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
    G4Colour colourFoil(1.0, 0.8, 1.0); //
    G4VisAttributes *foilVisAtt = new G4VisAttributes(true, colourFoil);
    foilLV->SetVisAttributes(foilVisAtt);
  }
  
  
  int flag = paramMan->GetEventFlag();
  int KDCFlag = paramMan->GetKDCFlag();
  int SSFlag = paramMan->GetSSFlag();
  G4RotationMatrix *rotSP = new G4RotationMatrix();
  rotSP->rotateY(57.468*degree); 
  G4RotationMatrix *rotDHKS = new G4RotationMatrix();
  rotDHKS->rotateY(70*degree);
  G4RotationMatrix *rotD = new G4RotationMatrix();
  rotD->rotateY(70*degree+Theta);
  G4RotationMatrix *rotDet = new G4RotationMatrix();
  //rotDet->rotateY(70*degree+Theta-1.0*degree);
  rotDet->rotateY(70*degree+Theta-0.0*degree);
  //rotDet->rotateX(-.18*degree);

  
  if( flag==0 || flag==1 ){ 
    //=== Splitter Magnet ===//
    G4ThreeVector posSplitter( 0.*cm,0.*cm,0.*cm );
    std::string Splittermap = paramMan->GetSplitterFieldMap();
    G4RotationMatrix *rotSPL = new G4RotationMatrix();
    rotSPL->rotateY(yawSPL); 
    rotSPL->rotateX(-pitchSPL); 
    rotSPL->rotateZ(-rollSPL); 
    SplitterMagnet *splitter =
      new SplitterMagnet( "Splitter", posSplitter, 
			  rotSPL, Splittermap );
    
    splitter->SetMaterials( mList_->Vacuum, mList_->Vacuum );
    splitter->Place( world );
    EMField_->AddElement( splitter );
    //=== Virtual Detector for Splitter Exit ===//
    G4double VD0SizeX = 400.*cm;
    G4double VD0SizeY = 400.*cm;
    G4double VD0SizeZ = 1.*cm;
    G4ThreeVector posVD0( 33.8*cm, 0.*cm, 93.35*cm );
    VirtualDetector *VD0 =
      new VirtualDetector( "VD0", posVD0, rotSP, 
			   VD0SizeX, VD0SizeY, VD0SizeZ, 0 );
    VD0->SetMaterials( mList_->Vacuum );
    //VD0->Place( world );
    
    
    //=== Target Chamber (only HKS fringe) ===//
    /*G4ThreeVector preposTC( 0, 0, 118.*cm );
      G4ThreeVector posTC = HKSTrans->TransformPoint(preposTC);
      TargetChamber *targetC =
      new TargetChamber( "TargetChabmer", posTC, rotHKS );
      targetC->SetMaterials( mList_->Fe, mList_->Vacuum );
      targetC->Place( world );
      EMField_->AddElement( targetC );*/
    
    
    std::string Q1map = paramMan->GetQ1FieldMap();
    //=== Quadrupole 1 ===//
    //G4double VTtoQ1 = (190.64+7.5)*cm;
    //G4double VTtoQ1 = 210.*cm;
    G4double VTtoQ1 = 208*cm;
    G4ThreeVector preposQ1( 0*cm, 0*cm, VTtoQ1 );
    preposQ1+=dposQ1;
    G4ThreeVector posQ1 = HKSTrans->TransformPoint(preposQ1);
   G4RotationMatrix *rotQ1 = new G4RotationMatrix();
   rotQ1 = rotHKS;
   rotQ1->rotateY(yawQ1); 
   rotQ1->rotateX(-pitchQ1); 
   rotQ1->rotateZ(-rollQ1); 
    Q1Magnet *Q1 = 
      new Q1Magnet( "Q1", posQ1, rotQ1, Q1map );
    Q1->SetMaterials( mList_->Fe, mList_->Vacuum );
    Q1->Place( world );
    EMField_->AddElement( Q1 );
    
    //== Collimator & Sieve slit ==//
    //G4double VTtoSS = VTtoQ1 - 42.*cm -17.3*cm + 2.54/2*cm;
    //G4double VTtoSS = VTtoQ1 - 42.*cm -49.51*cm+17.23*cm;
    //G4double VTtoSS = VTtoQ1 - 59.859*cm-2.54*cm;
    G4double VTtoSS = 149.2*cm-2.54*cm;
    G4ThreeVector preposSS( 0.0*cm, 0, VTtoSS );
    //G4ThreeVector preposSS( 1.5*cm, 0, VTtoSS );
    //G4ThreeVector preposSS( -0.199*cm, 0.004*cm, VTtoSS );
    G4ThreeVector posSS = HKSTrans->TransformPoint(preposSS);
    SieveSlit *Slit = new SieveSlit( "Sieve Slit", posSS, rotHKS );
    Collimator *Coll = new Collimator( "Collimator" , posSS, rotHKS , 0);
    if (SSFlag==1){ 
      Slit->SetMaterials( mList_->Heavymet, mList_->Vacuum );
      Slit->Place( world );
    }
    //EMField_->AddElement(Slit );
    else if (SSFlag==2) {
      Coll->SetMaterials( mList_->Heavymet, mList_->Vacuum );
      Coll->Place( world );
    }
    //EMField_->AddElement( Coll );
    
    
    //=== Virtual Detector for Q1 Entrance ===//
    //G4double VTtoVD1 = VTtoQ1 - 43.1*cm;
    G4double VTtoVD1 = VTtoQ1-42.*cm;
    G4ThreeVector preposVD1( 0.*cm, 0.*cm, VTtoVD1 );
    G4ThreeVector posVD1 = HKSTrans->TransformPoint(preposVD1);
    G4double VD1SizeX = 100.*cm;
    G4double VD1SizeY = 50.*cm;
    G4double VD1SizeZ = .1*cm;
    VirtualDetector *VD1 =
      new VirtualDetector( "VD1", posVD1, rotHKS, 
			   VD1SizeX, VD1SizeY, VD1SizeZ, 1 );
    VD1->SetMaterials( mList_->Vacuum );
    VD1->Place( world );
    
    //=== Virtual Detector for Q1 Exit ===//
    G4double VTtoVD2 = VTtoQ1 + 43.1*cm;
    G4double VD2SizeX = 100.*cm;
    G4double VD2SizeY = 50.*cm;
    G4double VD2SizeZ = 1.*cm;
    G4ThreeVector preposVD2( 0.*cm, 0.*cm, VTtoVD2 );
    G4ThreeVector posVD2 = HKSTrans->TransformPoint(preposVD2);
    VirtualDetector *VD2 =
      new VirtualDetector( "VD2", posVD2, rotHKS, 
			   VD2SizeX, VD2SizeY, VD2SizeZ, 2 );
    VD2->SetMaterials( mList_->Vacuum );
    VD2->Place( world );
    
    
    //=== Quadrupole 2 ===//
    G4double VTtoQ2 = VTtoQ1 + 102.*cm;
    G4ThreeVector preposQ2( 0.*cm, 0.*cm, VTtoQ2 );
    preposQ2+=dposQ2;
    G4ThreeVector posQ2 = HKSTrans->TransformPoint(preposQ2);
    std::string Q2map = paramMan->GetQ2FieldMap();
    G4RotationMatrix *rotQ2 = new G4RotationMatrix();
    rotQ2 = rotHKS;
    rotQ2->rotateY(yawQ2-1.*degree); 
    rotQ2->rotateX(-pitchQ2); 
    rotQ2->rotateZ(-rollQ2); 
    Q2Magnet *Q2 = new Q2Magnet( "Q2", posQ2, rotQ2, Q2map );
    Q2->SetMaterials( mList_->Fe, mList_->Vacuum );
    Q2->Place( world );
    EMField_->AddElement( Q2 );
    
    //=== Virtual Detector for Q2 Entrance ===//
    G4double VTtoVD3 = VTtoQ2 - 31.4*cm;
    G4ThreeVector preposVD3( 0.*cm, 0.*cm, VTtoVD3 );
    G4ThreeVector posVD3 = HKSTrans->TransformPoint(preposVD3);
    G4double VD3SizeX = 100.*cm;
    G4double VD3SizeY = 50.*cm;
    G4double VD3SizeZ = 0.1*cm;
    VirtualDetector *VD3 =
      new VirtualDetector( "VD3", posVD3, rotHKS, 
			   VD3SizeX, VD3SizeY, VD3SizeZ, 3 );
    VD3->SetMaterials( mList_->Vacuum );
    VD3->Place( world );
    
    //=== Virtual Detector for Q2 Exit ===//
    G4double VTtoVD4 = VTtoQ2 + 31.4*cm;
    G4ThreeVector preposVD4( 0.*cm, 0.*cm, VTtoVD4 );
    G4ThreeVector posVD4 = HKSTrans->TransformPoint(preposVD4);
    G4double VD4SizeX = 100.*cm;
    G4double VD4SizeY = 50.*cm;
    G4double VD4SizeZ = 0.1*cm;
    VirtualDetector *VD4 =
      new VirtualDetector( "VD4", posVD4, rotHKS, 
			   VD4SizeX, VD4SizeY, VD4SizeZ, 4 );
    VD4->SetMaterials( mList_->Vacuum );
    VD4->Place( world );
    
    //=== Dipole Magnet ==//
    G4double VTtoDen = VTtoQ2 + 70.*cm;
    //G4double DCenter = 270.0*cm;
    G4double DCenter = 266.3*cm+0*cm;
    G4ThreeVector preDen(  -DCenter, 0.*cm, VTtoDen  );
    preDen+=dposD;
    const G4ThreeVector posD = HKSTrans->TransformPoint(preDen);
    std::string Dmap = paramMan->GetDFieldMap();
    G4RotationMatrix *rotDmag = new G4RotationMatrix();
	 rotDmag->rotateY(35.*degree+Theta);
    rotDmag->rotateY(yawD); 
    rotDmag->rotateX(-pitchD); 
    rotDmag->rotateZ(-rollD); 
	 DMagnet *Dipole = new DMagnet( "D", posD, rotDmag ,Dmap );
    Dipole->SetMaterials( mList_->Fe, mList_->Vacuum );
    Dipole->Place( world );
    EMField_->AddElement( Dipole );
    
    //=== Virtual Detector for Dipole Entrance ===//
    G4double VD5SizeX = 200.*cm;
    G4double VD5SizeY = 400.*cm;
    G4double VD5SizeZ = 0.1*cm;
    G4double preposVD5X = 0.*cm;
    G4double preposVD5Y = preDen.y();
    G4double preposVD5Z = preDen.z();  
    G4ThreeVector preposVD5(preposVD5X,preposVD5Y,preposVD5Z);
    G4ThreeVector posVD5 = HKSTrans->TransformPoint(preposVD5);
    VirtualDetector *VD5 =
      new VirtualDetector( "VD5", posVD5, rotHKS, 
			   VD5SizeX, VD5SizeY, VD5SizeZ, 5 );
    VD5->SetMaterials( mList_->Vacuum );
    VD5->Place( world );
    
    //=== Virtual Detector for Dipole exit ===//
    G4double VD6SizeX = 200.*cm;
    G4double VD6SizeY = 400.*cm;
    G4double VD6SizeZ = 0.1*cm;
    G4ThreeVector preposVD6 = posVD5-posD;
    G4AffineTransform *rotVD = new G4AffineTransform( rotDHKS,posD );
    G4ThreeVector posVD6 = rotVD->TransformPoint(preposVD6);
    VirtualDetector *VD6 =
    new VirtualDetector( "VD6", posVD6, rotD,
			 VD6SizeX, VD6SizeY, VD6SizeZ, 6 );
    VD6->SetMaterials( mList_->Vacuum );
    VD6->Place( world );
    
    G4AffineTransform *DetTrans = new G4AffineTransform( rotD,G4ThreeVector(0.0,0.0,0.0) );
    //DriftChamber *DC1= new DriftChamber( "KDC1", 0, 0, 0 );
    //DriftChamber *DC2= new DriftChamber( "KDC2", 0, 0, 0 );


    if (KDCFlag!=0){ 
      
      //=== ChamberExtension ==//
      ChamberExtension *CExtension =
	new ChamberExtension( "Chamber Extension", posD, rotD );
      CExtension->SetMaterials( mList_->sus304, mList_->Vacuum, mList_->Kevlar );
      CExtension->Place( world );
      EMField_->AddElement(CExtension);
      
      ChamberExtension2 *CExtension2 =
	new ChamberExtension2( "Chamber Extension2", posD, rotD ); //Added by Gogami (29Oct2010)
      CExtension2->SetMaterials( mList_->sus304, mList_->Vacuum, mList_->Kevlar );
      CExtension2->Place( world );
      EMField_->AddElement(CExtension2);
      
      
      //=== NMRPort ===// Added by Gogami (2Nov2010)
      NMRPort *NMRP =new NMRPort( world , mList_ );
      G4double VD6toNMRP = 20.00*cm;
      G4double offNMRPflange = 7.375 ; // [cm]
      //High Momentum side
      G4ThreeVector VD6toNMRPD( (-144.45/2.0)*cm, 0.*cm,  VD6toNMRP);//tube
      G4ThreeVector preposNMRP = DetTrans->TransformPoint(VD6toNMRPD);
      G4ThreeVector posNMRP = preposNMRP + posVD6; 
      G4ThreeVector VD6toNMRPD2( (-144.45/2.0 - offNMRPflange )*cm, 0.*cm,  VD6toNMRP);//flange
      G4ThreeVector preposNMRP2 = DetTrans->TransformPoint(VD6toNMRPD2);
      G4ThreeVector posNMRP2 = preposNMRP2 + posVD6; 
      //Low Momentum side
      G4ThreeVector VD6toNMRPD3( ( 144.45/2.0)*cm, 0.*cm,  VD6toNMRP);//tube
      G4ThreeVector preposNMRP3 = DetTrans->TransformPoint(VD6toNMRPD3);
      G4ThreeVector posNMRP3 = preposNMRP3 + posVD6; 
      G4ThreeVector VD6toNMRPD4( ( 144.45/2.0 + offNMRPflange )*cm, 0.*cm,  VD6toNMRP);//flange
      G4ThreeVector preposNMRP4 = DetTrans->TransformPoint(VD6toNMRPD4);
      G4ThreeVector posNMRP4 = preposNMRP4 + posVD6; 
      G4RotationMatrix* RotNMRP = new G4RotationMatrix;
      RotNMRP->rotateY(70*degree+Theta+90*degree);
      NMRP -> Place( RotNMRP 
		     , posNMRP
		     , posNMRP2
		     , posNMRP3
		     , posNMRP4);

      //=== Chamber Extension Flange ===// Added by Gogami (2Nov2010)
      G4double VD6toCEF1 = 33.20*cm;
      G4double VD6toCEF2 = 34.40*cm;
      G4ThreeVector VD6toCEFD1( 0.0*cm, 0.0*cm,  VD6toCEF1);
      G4ThreeVector preposCEF1 = DetTrans->TransformPoint(VD6toCEFD1);
      G4ThreeVector posCEF1 = preposCEF1 + posVD6;
      G4ThreeVector VD6toCEFD2( 0.0*cm, 0.0*cm,  VD6toCEF2);
      G4ThreeVector preposCEF2 = DetTrans->TransformPoint(VD6toCEFD2);
      G4ThreeVector posCEF2 = preposCEF2 + posVD6;
      CEF *CEFlange = new CEF( world , mList_ );
      CEFlange -> Place(rotD,posCEF1,rotD,posCEF2);
      
      
      //=== Drift Chamber===//
      //      G4double VD6toDC1 = 195.*cm;
      G4double VD6toDC1 = 187.04*cm;
      //G4ThreeVector VD6toDCD1( 0.*cm, 0.*cm,  VD6toDC1);
      G4ThreeVector VD6toDCD1( -5.*cm, -2.*cm,  VD6toDC1);
      G4ThreeVector preposDC1 = DetTrans->TransformPoint(VD6toDCD1);
      G4ThreeVector posDC1 = preposDC1 + posVD6; 
      DriftChamber* DC1 = new DriftChamber( "KDC1", posDC1, rotD, 0 );
      DC1->SetMaterials( mList_->Vacuum, mList_->DCGas,
			 mList_->Mylar, mList_->DCGas );
      DC1->Place(world);
      
      //      G4double VD6toDC2 = 295.*cm;
      G4double VD6toDC2 = 287.04*cm;
      //G4ThreeVector VD6toDCD2( 0.*cm, 0.*cm,  VD6toDC2);
      G4ThreeVector VD6toDCD2( -5.*cm, -2.*cm,  VD6toDC2);
      G4ThreeVector preposDC2 = DetTrans->TransformPoint(VD6toDCD2);
      G4ThreeVector posDC2 = preposDC2 + posVD6;
      DriftChamber* DC2 = new DriftChamber( "KDC2", posDC2, rotD, 1 );
      DC2->SetMaterials( mList_->Vacuum, mList_->DCGas,
			 mList_->Mylar, mList_->DCGas );
      //DC2->SetMaterials( mList_->Vacuum, mList_->Vacuum,
      // 	  mList_->Vacuum, mList_->Vacuum );
      DC2->Place(world );
      HKSDCSD *DCSD = new HKSDCSD( "/HKS/DC/" );
      SDManager->AddNewDetector( DCSD );
      DC1->GetDetectorLV()->SetSensitiveDetector( DCSD );
      DC2->GetDetectorLV()->SetSensitiveDetector( DCSD );
      DCSD->SetElement( 0, DC1 );
      DCSD->SetElement( 1, DC2 );
      
      //=== He Gas between DC1 & DC2 ===//
      G4String nmHe = "He Bag";
      G4ThreeVector dxyz_HeBag( 130.*cm, 35.*cm, 94.87*cm );
      G4Box *HeBagSolid = new G4Box( nmHe,
				     dxyz_HeBag.x()/2,//modified by Gogami
				     dxyz_HeBag.y()/2,//modified by Gogami
				     dxyz_HeBag.z()/2 );
      //      G4double VD6toHe = 245.*cm;
      G4double VD6toHe = 237.04*cm;
      //G4ThreeVector VD6toHeD( 0.*cm, 0.*cm,  VD6toHe);
      G4ThreeVector VD6toHeD( -5.*cm, 0.*cm,  VD6toHe);
      G4ThreeVector preposHeBag1 = DetTrans->TransformPoint(VD6toHeD);
      G4ThreeVector posHeBag1 = preposHeBag1 + posVD6; 
      
      G4LogicalVolume *HeBagLV = 
	//new G4LogicalVolume( HeBagSolid, mList_->Vacuum, nmHe );
	new G4LogicalVolume( HeBagSolid, mList_->HeGas, nmHe );
      
      G4PVPlacement *HeBag =
	new G4PVPlacement(rotD, posHeBag1, nmHe, HeBagLV,
			  world, false, 0 );
      
      // Mylar of He Bag
      G4String nmHeMylar = "Mylar of He bag";
      //   G4ThreeVector dxyz_HeMylar( 130.*cm, 35.*cm, 0.127*mm );
      G4ThreeVector dxyz_HeMylar( 130.*cm, 35.*cm, 0.0254*mm );
      G4Box *HeMylarSolid = new G4Box( nmHeMylar,
				       dxyz_HeMylar.x()/2.0,
				       dxyz_HeMylar.y()/2.0,
				       dxyz_HeMylar.z()/2.0 );
      G4LogicalVolume *HeMylarLV =
	//new G4LogicalVolume( HeMylarSolid, mList_->Vacuum, nmHeMylar );
	new G4LogicalVolume( HeMylarSolid, mList_->Mylar, nmHeMylar );
      
      //double x=0.*cm,y=0.0*cm; 
      double x=-5.*cm,y=0.0*cm; 
      double z = 47.4*cm;
      G4ThreeVector posHeMy1( x, y, z );
      
      new G4PVPlacement( 0, posHeMy1, nmHeMylar, HeMylarLV,
			 HeBag, false, 0 );
      
      x = -5.*cm;
      y = 0.0*cm; z = -47.4*cm;
      G4ThreeVector posHeMy2( x, y, z );
      
      new G4PVPlacement( 0, posHeMy2, nmHeMylar, HeMylarLV,
			 HeBag, false, 1 );
    }
    
    //=== Virtual Detector for Detector Plane ===//
    
    G4double VD6toVD7 = 190.*cm;
    //   Modified by Gogami//////////////////////////
    //G4double VD6toVD7 = 10.*cm; //Test for porsitron distribution
    ////////////////////////////////////////////////
    
    G4ThreeVector VD6to7( 0.*cm, 0.*cm,  VD6toVD7);
    //G4ThreeVector VD6to7( -5.*cm, 0.*cm,  VD6toVD7);
    G4ThreeVector preposVD7 = DetTrans->TransformPoint(VD6to7);
    G4ThreeVector posVD7(preposVD7.x()+posVD6.x(),
			 preposVD7.y()+posVD6.y(),
			 preposVD7.z()+posVD6.z());
    G4double VDSizeX = 200.*cm;
    G4double VDSizeY = 100.*cm;
    G4double VDSizeZ = .01*cm;
    VirtualDetector *VD7 =
      new VirtualDetector( "VD7", posVD7, rotDet,
			   VDSizeX, VDSizeY, VDSizeZ, 7 );
    VD7->SetMaterials( mList_->Vacuum );
    VD7->Place( world );
    
    G4double VD6toVD8 = 235.*cm;
    G4ThreeVector VD6to8( 0.*cm, -2.*cm,  VD6toVD8);
    //G4ThreeVector VD6to8( -5.*cm, 0.*cm,  VD6toVD8);
    G4ThreeVector preposVD8 = DetTrans->TransformPoint(VD6to8);
    G4ThreeVector posVD8(preposVD8.x()+posVD6.x(),
			 preposVD8.y()+posVD6.y(),
			 preposVD8.z()+posVD6.z());
    VirtualDetector *VD8 =
      new VirtualDetector( "VD8", posVD8, rotDet,
			   VDSizeX, VDSizeY, VDSizeZ, 8 );
    VD8->SetMaterials( mList_->Vacuum );
    VD8->Place( world );
    
    
    G4double VD6toVD9 = 285.*cm;
    G4ThreeVector VD6to9( 0.*cm, 0.*cm,  VD6toVD9);
    //G4ThreeVector VD6to9( -5.*cm, 0.*cm,  VD6toVD9);
    G4ThreeVector preposVD9 = DetTrans->TransformPoint(VD6to9);
    G4ThreeVector posVD9(preposVD9.x()+posVD6.x(),
			 preposVD9.y()+posVD6.y(),
			 preposVD9.z()+posVD6.z());
    VirtualDetector *VD9 =
      new VirtualDetector( "VD9", posVD9, rotDet,
			   VDSizeX, VDSizeY, VDSizeZ, 9 );
    VD9->SetMaterials( mList_->Vacuum );
    VD9->Place( world );
    

    
    //====VD ====//
    
    G4double VD6toVD10 = 304.*cm;
    G4ThreeVector VD6to10( -5.0*cm, 0.*cm,  VD6toVD10);
    G4ThreeVector preposVD10 = DetTrans->TransformPoint(VD6to10);
    G4ThreeVector posVD10(preposVD10.x()+posVD6.x(),
			  preposVD10.y()+posVD6.y(),
			  preposVD10.z()+posVD6.z());
    VirtualDetector *VD10 =
      new VirtualDetector( "VD10", posVD10, rotDet,
			   VDSizeX, VDSizeY, VDSizeZ, 10 );
    VD10->SetMaterials( mList_->Vacuum );
    VD10->Place( world );

    
    //=== HTOF 1X ===//
    
    //    G4double VD6toT1X = 300.*cm;
    //    G4ThreeVector VD6toT1X( 0.*cm, 0.*cm,  300.*cm);
    G4ThreeVector VD6toT1X( -5.0*cm, 0.*cm,  306.24*cm);
    G4ThreeVector preposT1X = DetTrans->TransformPoint(VD6toT1X);
    G4ThreeVector posT1X(preposT1X.x()+posVD6.x(),
			  preposT1X.y()+posVD6.y(),
			  preposT1X.z()+posVD6.z());
    HTOF1X *Htof1x = new HTOF1X( "HTOF 1x", posT1X, rotD, 0 );
    Htof1x->SetMaterial( mList_->Scinti );
    Htof1x->Place(world);
    EMField_->AddElement(Htof1x);
    
    
    //=== HTOF 1Y ===//
    //    G4double VD6toH1Y = 315.*cm;
    G4double T1XtoH1Y = 16.03*cm;
    //    G4ThreeVector VD6to1Y( 0.*cm, 0.*cm,  VD6toH1Y);
    //    G4ThreeVector VD6to1Y( -5.0*cm, 0.*cm,  VD6toH1Y);
    G4ThreeVector T1Xto1Y( 0.0*cm, 0.*cm,  T1XtoH1Y);
    //    G4ThreeVector prepos1Y = DetTrans->TransformPoint(VD6to1Y);
    G4ThreeVector prepos1Y = DetTrans->TransformPoint(VD6toT1X+T1Xto1Y);
    G4ThreeVector pos1Y = prepos1Y+posVD6;  
    HTOF1Y *Htof1y = new HTOF1Y( "HTOF 1y", pos1Y, rotD, 0 );
    Htof1y->SetMaterial( mList_->Scinti );
    Htof1y->Place(world);
    EMField_->AddElement(Htof1y);
    
    //=== Aerogel Cherenkov Counter ===//
    //x = -3.67*cm;
    //y = 0.*cm;
    //z = -30.*cm;
    //    G4ThreeVector DetoposAC1( 0.*cm, 0.*cm, 345.*cm );
    //    G4double T1XtoAC1 = 42.217*cm;
    G4ThreeVector T1XtoAC1( 7.5*cm, 0.*cm, 42.218*cm );
    //    G4ThreeVector DetoposAC1( -1.2*cm, 0.*cm, 347.191*cm ); 
    //    G4ThreeVector DetoposAC1( -1.2*cm, 0.*cm, 345.*cm ); 
    //    G4ThreeVector DetoposAC1( -3.7*cm, 0.*cm, 345.*cm );
    //    G4ThreeVector preposAC1 = DetTrans->TransformPoint(DetoposAC1);
    G4ThreeVector preposAC1 = DetTrans->TransformPoint(VD6toT1X+T1XtoAC1);
    G4ThreeVector posAC1 = preposAC1 + posVD6;  
    AerogelC *AC1 = new AerogelC( "AC1", posAC1, rotD, 0 );
     AC1->SetMaterials( mList_->Vacuum, mList_->Vacuum, mList_->AC );
    AC1->Place(world );
    
    //x = -7.5*cm;
    //y = 0.*cm;
    //z = 1.8*cm;
    //G4ThreeVector AC1toposAC2( 0, 0, 30.*cm );
    G4ThreeVector AC1toposAC2( -7.5*cm, 0*cm, 30.8*cm );
    //    G4ThreeVector AC1toposAC2( -3.8*cm, 0*cm, 30.*cm );
    G4ThreeVector preposAC2 = DetTrans->TransformPoint(AC1toposAC2);
    G4ThreeVector posAC2 = preposAC2 + posAC1;  
    AerogelC *AC2 = new AerogelC( "AC2", posAC2, rotD, 1 );
    AC2->SetMaterials( mList_->Vacuum, mList_->Vacuum, mList_->AC );
    AC2->Place( world );
    
    //x = -11.3*cm;
    //y = 0.*cm;
    //z = 32.6*cm;
    //G4ThreeVector AC2toposAC3( 0, 0, 30.*cm );
    G4ThreeVector AC2toposAC3( -7.5*cm, 0*cm, 30.8*cm );
    G4ThreeVector preposAC3 = DetTrans->TransformPoint(AC2toposAC3);
    G4ThreeVector posAC3 = preposAC3 + posAC2;  
    AerogelC *AC3 = new AerogelC( "AC3", posAC3, rotD, 2 );
    AC3->SetMaterials( mList_->Vacuum, mList_->Air, mList_->AC );
    AC3->Place( world );
    
    
    //=== HTOF 2X ===//
    //    G4double VD6toH2X = 450.*cm;
    G4double T1XtoH2X = 150.*cm;
    //G4ThreeVector VD6to2X( 0.*cm, 0.*cm,  VD6toH2X);
    //    G4ThreeVector VD6to2X( -5.0*cm, 0.*cm,  VD6toH2X);
    G4ThreeVector T1Xto2X( -2.5*cm, 0.*cm,  T1XtoH2X);//need to be checked.
    //    G4ThreeVector prepos2X = DetTrans->TransformPoint(VD6to2X);
    G4ThreeVector prepos2X = DetTrans->TransformPoint(VD6toT1X+T1Xto2X);
    G4ThreeVector pos2X=prepos2X+posVD6;
    HTOF2X *Htof2x = new HTOF2X( "HTOF 2x", pos2X, rotD, 0 );
    Htof2x->SetMaterial( mList_->Scinti );
    Htof2x->Place(world);
    EMField_->AddElement(Htof2x);

    
    //===W or F Cherenkov Counter===//
    
    //  G4double x1 = -7.5*cm;
    //    G4double z1 = 461*cm;//maybe incorrect
    G4double T1XtoW1 = 161.184*cm;

    //    G4ThreeVector DexittoWC1(0, 0, z1);
    //    G4ThreeVector DexittoWC1(3.9*cm, 0, z1); 
    //    G4ThreeVector DexittoWC1(-5.0*cm, 0*cm, z1);
    //    G4ThreeVector DexittoWC1(-7.5*cm, 0*cm, z1);
    G4ThreeVector T1XtoWC1( 0.275*cm, 0.*cm,  T1XtoW1);
    G4ThreeVector preposWC1 = DetTrans->TransformPoint(VD6toT1X+T1XtoWC1);
    G4ThreeVector posWC1 = preposWC1+posVD6;  
    //    G4ThreeVector preposWC1 = DetTrans->TransformPoint(DexittoWC1);
    //    G4ThreeVector posWC1 = preposWC1 + posVD6;  
    WaterC *WC1 = new WaterC("WC1",posWC1, rotD ,0);
    WC1->SetMaterials(mList_->Vacuum,mList_->Vacuum, mList_->Water);
    WC1->Place( world );
    
    //G4double x2 = -7.5*cm;
    //G4double z2 = 4.3*cm;//maybe correct
    //G4ThreeVector WC1toWC2(x2, 0, z2) ;
    //    G4double z2 = 8.6*cm;//maybe correct
    //    G4double z2 = (8.6+0.75*2.54)*cm;
    G4double w1tow2 = 10.897*cm;//need to be checked.
    G4ThreeVector WC1toWC2(-7.95*cm, 0, w1tow2) ;
    //G4ThreeVector WC1toWC2(-7.8-7.5*cm, 0*cm, z2) ;
    G4ThreeVector preposWC2 = DetTrans->TransformPoint(WC1toWC2);
    WaterC *WC2 = new WaterC("WC2",preposWC2+posWC1, rotD ,1);
    WC2->SetMaterials(mList_->Vacuum,mList_->Vacuum, mList_->Water);
    WC2->Place( world);
    
    
    //====VD ====//
    //    G4double  T1XtoVD11 =148.5 *cm; //position of 2X
    //G4double VD6toVD11 = 482.*cm;
    G4double T1XtoVD11 = 186.76*cm;//position of lucite
    //    G4double VD6toVD11 = 455.*cm;//position of 2X
    G4double VD6toVD11 = 306.24*cm + T1XtoVD11;
    //    G4ThreeVector VD6to11( -5.0*cm, 0.*cm,  VD6toVD11);
    //G4ThreeVector VD6to11( -2.5*cm, 0.*cm,  VD6toVD11); //2X
    G4ThreeVector VD6to11( -6.478*cm, 0.*cm,  VD6toVD11); //lucite
    G4ThreeVector preposVD11 = DetTrans->TransformPoint(VD6to11);
    G4ThreeVector posVD11(preposVD11.x()+posVD6.x(),
			  preposVD11.y()+posVD6.y(),
			  preposVD11.z()+posVD6.z());  
    VirtualDetector *VD11 =
      new VirtualDetector( "VD11", posVD11, rotD,
			   VDSizeX, VDSizeY, VDSizeZ, 11 );
    VD11->SetMaterials( mList_->Vacuum );
    VD11->Place( world );
    
    //=======Virtual Detector for SPL=======//
    
    G4ThreeVector preposVD12( 0.*cm, 0.*cm, 32.5*cm );
    G4ThreeVector posVD12 = HKSTrans->TransformPoint(preposVD12);
    G4double VD12SizeX = 300.*cm;
    G4double VD12SizeY = 100.*cm;
    G4double VD12SizeZ = 1.*cm;
    VirtualDetector *VD12 =
      new VirtualDetector( "VD12", posVD12, rotHKS, 
			   VD12SizeX, VD12SizeY, VD12SizeZ, 12 );
    VD12->SetMaterials( mList_->Vacuum );
    VD12->Place( world );
    
    
    //G4ThreeVector preposVD13( 0.*cm, 0.*cm, 57.*cm );
    //G4ThreeVector posVD13 = HKSTrans->TransformPoint(preposVD13);
    G4ThreeVector posVD13 = posSS;
    G4double VD13SizeX = 100.*cm;
    G4double VD13SizeY = 100.*cm;
    G4double VD13SizeZ = 0.01*cm;
    VirtualDetector *VD13 =
      new VirtualDetector( "VD13", posVD13, rotHKSSS, 
			   VD13SizeX, VD13SizeY, VD13SizeZ, 13 );
    VD13->SetMaterials( mList_->Vacuum );
    VD13->Place( world );
    
    
    G4ThreeVector preposVD14( 0.*cm, 0.*cm, 82.*cm );
    G4ThreeVector posVD14 = HKSTrans->TransformPoint(preposVD14);
    G4double VD14SizeX = 300.*cm;
    G4double VD14SizeY = 100.*cm;
    G4double VD14SizeZ = 1.*cm;
    VirtualDetector *VD14 =
      new VirtualDetector( "VD14", posVD14, rotHKS, 
			   VD14SizeX, VD14SizeY, VD14SizeZ, 14 );
    VD14->SetMaterials( mList_->Vacuum );
    VD14->Place( world );
    
    /*
      G4double VD6toVD12 = 440.*cm;
      G4ThreeVector VD6to12( 0.*cm, 0.*cm,  VD6toVD12);
      G4ThreeVector preposVD12 = DetTrans->TransformPoint(VD6to12);
      G4ThreeVector posVD12(preposVD12.x()+posVD6.x(),
      preposVD12.y()+posVD6.y(),
      preposVD12.z()+posVD6.z());
      VirtualDetector *VD12 =
      new VirtualDetector( "VD12", posVD12, rotD,
      VDSizeX, VDSizeY, VDSizeZ, 12 );
      VD12->SetMaterials( mList_->Vacuum );
      VD12->Place( world );
      
      G4double VD6toVD13 = 450.*cm;
      G4ThreeVector VD6to13( 0.*cm, 0.*cm,  VD6toVD13);
      G4ThreeVector preposVD13 = DetTrans->TransformPoint(VD6to13);
      G4ThreeVector posVD13(preposVD13.x()+posVD6.x(),
      preposVD13.y()+posVD6.y(),
      preposVD13.z()+posVD6.z());
      VirtualDetector *VD13 =
      new VirtualDetector( "VD13", posVD13, rotD,
      VDSizeX, VDSizeY, VDSizeZ, 13 );
      VD13->SetMaterials( mList_->Vacuum );
      VD13->Place( world );
    */
    /////////////////////////
    // Sensitive Detectors //
    /////////////////////////
    
    //=== Virtual Detector ===//
    HKSVDSD *VDSD = new HKSVDSD( "/HKS/VD" );
    SDManager->AddNewDetector( VDSD );
    //VD0->GetDetectorLV()->SetSensitiveDetector( VDSD );
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
    
    // VDSD->SetElement( 0, VD0 );
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
    
    /*#ifdef SIEVE_SLIT
    //=== Sieve Slit ===//
    HKSSSSD *SSSD = new HKSSSSD( "/HKS/SS" );
    SDManager->AddNewDetector( SSSD );
    Slit->SetSD( SSSD );
    SSSD->SetElement( Slit );
    #else
    VDSD->SetElement( 7, Coll );
    Coll->GetDetectorLV()->SetSensitiveDetector( VDSD );
    #endif*/
    
    
    //=== Target ==//
    HKSTargetSD *targetSD = new HKSTargetSD( "/HKS/Target/" );
    SDManager->AddNewDetector( targetSD );
    targetLV->SetSensitiveDetector( targetSD );
    
    
    //===W or F Cherenkov Counter===//
    HKSWCSD *WCSD = new HKSWCSD( "HKS/WC" );
    SDManager->AddNewDetector( WCSD );
    WC1->GetDetectorLV()->SetSensitiveDetector( WCSD );
    WC2->GetDetectorLV()->SetSensitiveDetector( WCSD );
    WCSD->SetElement( 0, WC1 );
    WCSD->SetElement( 1, WC2 );
    
    //=== HTOF ===
    HKSTOFSD *tofSD = new HKSTOFSD( "/HKS/TOF/" );
    SDManager->AddNewDetector( tofSD );
    Htof1x->GetDetectorLV()->SetSensitiveDetector( tofSD );
    Htof1y->GetDetectorLV()->SetSensitiveDetector( tofSD );
    Htof2x->GetDetectorLV()->SetSensitiveDetector( tofSD );
    tofSD->SetElement( 0, Htof1x );
    tofSD->SetElement( 1, Htof1y );
    tofSD->SetElement( 2, Htof2x );
    
    //=== Aerogel Cherenkov Counter ===//
    HKSACSD *ACSD = new HKSACSD( "/HKS/AC/" );
    SDManager->AddNewDetector( ACSD );
    AC1->GetDetectorLV()->SetSensitiveDetector( ACSD );
    AC2->GetDetectorLV()->SetSensitiveDetector( ACSD );
    AC3->GetDetectorLV()->SetSensitiveDetector( ACSD );
    ACSD->SetElement( 0, AC1 );
    ACSD->SetElement( 1, AC2 );
    ACSD->SetElement( 2, AC3 );
    
//    if (KDCFlag!=0){
//      //=== Drift Chamber ===//
//      HKSDCSD *DCSD = new HKSDCSD( "/HKS/DC/" );
//      SDManager->AddNewDetector( DCSD );
//      DC1->GetDetectorLV()->SetSensitiveDetector( DCSD );
//      DC2->GetDetectorLV()->SetSensitiveDetector( DCSD );
//      DCSD->SetElement( 0, DC1 );
//      DCSD->SetElement( 1, DC2 );
//    }
  }//(flag==0 || flag==1)
  
  
  ///////////////////
  // Visualisation //
  ///////////////////
  
  //=== World ===//
  worldLV->SetVisAttributes(G4VisAttributes::Invisible);
  
  //=== Splitter Yoke ===//
  
  //=== Q1 Yoke ===//

  //=== Q2 Yoke ===//

  //=== Target ===//
  /*G4Colour colourTarget(0., 1., 1.); // cyan
  G4VisAttributes *targetVisAtt = new G4VisAttributes(true, colourTarget);
  targetLV->SetVisAttributes(targetVisAtt);*/



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
  
  //delete rotSP;
  //delete rotDHKS;
  //delete rotD;
  delete rotHKS;
  return world;
}

