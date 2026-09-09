/*
  NMR Ports of HKS
  2 Nov 2010
  T.Gogami
*/

#include "NMRPort.hh"
#include "MaterialList.hh"

#include "G4VPhysicalVolume.hh"
#include "G4RotationMatrix.hh"
#include "G4LogicalVolume.hh"
#include "G4SubtractionSolid.hh"
#include "G4Tubs.hh"
#include "G4PVPlacement.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"

NMRPort::NMRPort(G4VPhysicalVolume* pMother , MaterialList* ml)
  :mother(pMother),mList(ml)
{}
NMRPort::~NMRPort()
{}

void NMRPort::Place( G4RotationMatrix* rot 
		     ,G4ThreeVector pos1
		     ,G4ThreeVector pos2
		     ,G4ThreeVector pos3
		     ,G4ThreeVector pos4 )
{
  G4String nameTube1 =  "NMRP_Tube1";
  G4String nameTube2 =  "NMRP_Tube2";
  G4String nameGap   = "NMRP_Gap" ;
  
  G4Tubs *Tubs1 = new G4Tubs(nameTube1 
			     , ( 2.22/2.0)*cm
			     , ( 6.05/2.0)*cm
			     , (11.25/2.0)*cm
			     , 0
			     , 360.0*deg);
  G4Tubs *Tubs2 = new G4Tubs(nameTube2 
			     , ( 2.22/2.0)*cm
			     , (11.40/2.0)*cm
			     , ( 3.50/2.0)*cm
			     , 0
			     , 360.0*deg);
  
  G4Tubs *Gap = new G4Tubs(nameGap 
			   , 0
			   , ( 2.22/2.0)*cm
			   , (11.25/2.0)*cm
			   , 0
			   , 360.0*deg);
  
  //Logical Volume
  G4LogicalVolume *NMRPort_LV1 =
    new G4LogicalVolume( Tubs1 , mList->sus304 , "NMRP_TubeBody1 LV" );
  G4LogicalVolume *NMRPort_LV2 =
    new G4LogicalVolume( Tubs2 , mList->sus304 , "NMRP_TubeBody2 LV" );
  G4LogicalVolume *Gap_LV =
    new G4LogicalVolume( Gap  , mList->Vacuum , "NMRP_Gap LV" );

  //Placement for Low Momentum side 
  new G4PVPlacement(rot
		    , pos1
		    , "NMR Port1"
		    , NMRPort_LV1
		    , mother
		    , false
		    , 0 );
  new G4PVPlacement(rot
		    , pos1
		    , "NMR Port1 Gap"
		    , Gap_LV
		    , mother
		    , false
		    , 0 );
  new G4PVPlacement(rot
		    , pos2
		    , "NMR Port1 flange"
		    , NMRPort_LV2
		    , mother
		    , false
		    , 0 );
  
  //Placement for High Momentum side
  new G4PVPlacement(rot
		    , pos3
		    , "NMR Port2"
		    , NMRPort_LV1
		    , mother
		    , false
		    , 1 );
  new G4PVPlacement(rot
		    , pos3
		    , "NMR Port2 Gap"
		    , Gap_LV
		    , mother
		    , false
		    , 1 );
  new G4PVPlacement(rot
		    , pos4
		    , "NMR Port2 flange"
		    , NMRPort_LV2
		    , mother
		    , false
		    , 1 );
  
  //Visual Attributes
  G4Colour colourNMRport(0.3, 0.3, 0.3); 
  G4VisAttributes *NMRportVisAtt = new G4VisAttributes(true, colourNMRport);
  NMRPort_LV1 -> SetVisAttributes(NMRportVisAtt);
  NMRPort_LV2 -> SetVisAttributes(NMRportVisAtt);
  Gap_LV      -> SetVisAttributes(G4VisAttributes::Invisible);
  
  
}
