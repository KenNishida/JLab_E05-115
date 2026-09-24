/*
  Chamber Extension Flange
  2 Nov 2010
  T.Gogami
*/

#include "CEF.hh"
#include "MaterialList.hh"

#include "G4VPhysicalVolume.hh"
#include "G4RotationMatrix.hh"
#include "G4LogicalVolume.hh"
#include "G4SubtractionSolid.hh"
#include "G4Box.hh"
#include "G4PVPlacement.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"

CEF::CEF(G4VPhysicalVolume* pMother , MaterialList* ml)
  :mother(pMother),mList(ml)
{}
CEF::~CEF()
{}

void CEF::Place(G4RotationMatrix*rot1,G4ThreeVector pos1,G4RotationMatrix*rot2,G4ThreeVector pos2)
{
  G4Box* CEFbox1 = new G4Box( "CEF box1"
			      , (147.4/2.0)*cm
			      , ( 35.4/2.0)*cm
			      , (  1.2/2.0)*cm );
  G4Box* CEFbox2 = new G4Box( "CEF box2"
			      , (133.2/2.0)*cm
			      , ( 24.2/2.0)*cm
			      , (  2.2/2.0)*cm );//(  1.2/2.0)*cm );
  G4Box* CEFbox3 = new G4Box( "CEF box3"
			      , (134.0/2.0)*cm
			      , ( 26.6/2.0)*cm
			      , (  2.2/2.0)*cm );//(  1.2/2.0)*cm );
  G4SubtractionSolid* CEF1_solid =
    new G4SubtractionSolid("CEFbox1-CEFbox2",CEFbox1,CEFbox2);
  G4SubtractionSolid* CEF2_solid =
    new G4SubtractionSolid("CEFbox1-CEFbox3",CEFbox1,CEFbox3);
  
  //Logical Volume
  G4LogicalVolume* CEF_LV1 =
    new G4LogicalVolume(CEF1_solid , mList->sus304 , "CEF LV1");
  G4LogicalVolume* CEF_LV2 =
    new G4LogicalVolume(CEF2_solid , mList->sus304 , "CEF LV2");
  
  //Placement
  new G4PVPlacement( rot1           //Rotation
		     ,pos1          //Position
		     , "CE Flange1" //its name
		     , CEF_LV1      //its Logical Volume
		     , mother       //its mother Volume
		     , false        
		     ,0 );          //Copy number        
  
  new G4PVPlacement( rot2           //Rotation		 
		     ,pos2 	    //Position		 
		     , "CE Flange2" //its name		 
		     , CEF_LV2	    //its Logical Volume 
		     , mother	    //its mother Volume	 
		     , false	    			 
		     ,0 );	    //Copy number        
  
  //Visual Attributes
  G4Colour colourCEF1(0.2, 0.2, 0.2);
  G4VisAttributes *CEFVisAtt1 = new G4VisAttributes(true, colourCEF1);
  CEF_LV1 -> SetVisAttributes(CEFVisAtt1);
  G4Colour colourCEF2(0.4, 0.4, 0.4); 
  G4VisAttributes *CEFVisAtt2 = new G4VisAttributes(true, colourCEF2);
  CEF_LV2 -> SetVisAttributes(CEFVisAtt2);
  
}
