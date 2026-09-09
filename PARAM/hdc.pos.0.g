; For HKS DC in real experiment
; L.Yuan 10/05/2004

; Number of layers installed in HKS detector setup
      hdc_num_layers = 12

; Number of chambers installed in HKS detector setup
      hdc_num_chambers = 2

; Z positions of various layers in HKS chambers
; hdc_n_zpos is the surveyed Z position of the center of chamber n.

    hdc_1_zpos = -47.96
    hdc_2_zpos = -47.96 + 100.171 
;the distance between HDC1 and HDC2 is 100.171 which is from post survey

    hdc_zpos   = hdc_1_zpos - 1.905,  
                 hdc_1_zpos - 1.270,  
                 hdc_1_zpos - 0.635,  
                 hdc_1_zpos + 0.3175, 
                 hdc_1_zpos + 0.9525, 
                 hdc_1_zpos + 1.5875, 
                 hdc_2_zpos - 1.905, 
                 hdc_2_zpos - 1.270, 
                 hdc_2_zpos - 0.635, 
                 hdc_2_zpos + 0.3175,
                 hdc_2_zpos + 0.9525,
                 hdc_2_zpos + 1.5875

; Angle alpha of wires in wire chamber layers (.051,.045 degrees roll in dc1,2)
;    hdc_alpha_angle =( 30)*raddeg,
;                     ( 30)*raddeg,
;                     ( 90)*raddeg,      
;                     ( 90)*raddeg,     
;                     (150)*raddeg,    
;                     (150)*raddeg,
;                     ( 30)*raddeg,
;                     ( 30)*raddeg,
;                     ( 90)*raddeg,
;                     ( 90)*raddeg,
;                     (150)*raddeg,
;                     (150)*raddeg
    hdc_alpha_angle =(150)*raddeg,
                     (150)*raddeg,
                     ( 90)*raddeg,      
                     ( 90)*raddeg,     
                     ( 30)*raddeg,    
                     ( 30)*raddeg,
                     (150)*raddeg,
                     (150)*raddeg,
                     ( 90)*raddeg,
                     ( 90)*raddeg,
                     ( 30)*raddeg,
                     ( 30)*raddeg

; Angle beta of wires in wire chamber layers
    hdc_beta_angle =   0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,
                       0.0*raddeg,

; Angle gamma of wires in wire chamber layers
    hdc_gamma_angle = 0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg,
                      0.0*raddeg
; Pitch
    hdc_pitch = 1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000,
                1.0000

; Number of wires per layer
    hdc_nrwire = 87
                 87
                122
                122
                 87
                 87
                 87	
                 87
                122
                122
                 87
                 87

; X,Y position of center of wire chamber
; The given value is subtracted from the position of the wire (i.e.
; the sign is opposite of the actual center position of the chamber).
; Increasing dc2 coordinates increases dpos histograms.
; One value per layer for HKS DC.    L.Yuan 07/03/2003
; DC2 xcenter +0.474 cm from dx vs. xp correlation.  L.Yuan 03/18/04

; DC x center is now staying at x=-5 in the HKS cordinate. MK, 2005/06/02

    hdc_xcenter =  0.0 -(-5.0)-0.02863+0.04716-0.0015,
                   0.0 -(-5.0)+0.01817+0.05223+0.0004,
                   0.0 -(-5.0)-0.01+0.05477+0.0015,
                   0.0 -(-5.0)-0.0032+0.06590-0.0001,
                   0.0 -(-5.0)-0.0345+0.06997-0.0007,
                   0.0 -(-5.0)+0.022+0.07709+0.0003,
                   0.0 -(-5.0)-0.08-0.0305+0.84635+0.0021,
                   0.0 -(-5.0)-0.08+0.84208,
                   0.0 -(-5.0)-0.08-0.0085+0.85648+0.0013,
                   0.0 -(-5.0)-0.08+0.0033+0.86408-0.0004,
                   0.0 -(-5.0)-0.08-0.0324+0.86922+0.0035,
                   0.0 -(-5.0)-0.08+0.0107+0.86725

    hdc_ycenter =  0.0+0.0069+0.1572,
                   0.0+0.001+0.1583,
                   0.0+0.1594,
                   0.0+0.1611,
                   0.0-0.0049+0.1622,
                   0.0+0.0012+0.1633,
                   0.0-1.07-0.0007+0.33302,
                   0.0-1.07+0.3353+0.0034,
                   0.0-1.07+0.3369,
                   0.0-1.07+0.3369,
                   0.0-1.07-0.0016+0.3380,
                   0.0-1.07-0.0007+0.3407-0.0057

; Wire number of center of wire chamber
; Note the convention : x : low number =  -x , u,v : low number = "-x"
; x1 and x2, u1 and u2, v1 and v2 are offset by half a cell length
; assume 1st wire of 1st dc of a set is closest to edge where counting starts

      hdc_central_wire = (43.75),
                         (44.25),
                         (61.75),
                         (61.25),
                         (43.75),
                         (44.25),
                         (43.75),
                         (44.25),
                         (61.75),
                         (61.25),
                         (43.75),
                         (44.25)

; array giving the chamber number for each layer
      hdc_chamber_layers = 1,
                           1,
                           1,
                           1,
                           1,
                           1,
                           2,
                           2,
                           2,
                           2,
                           2,
                           2

; The following array is a flag on the order number.
; If hdc_wire_counting(plane) = 0
;  the wire center is at (wire - hdc_central_wire) * pitch
; If hdc_wire_counting(plane) = 1
;  the wire center is at ( hdc_nrwire + 1 - wire - hdc_central_wire) * pitch
        hdc_wire_counting = 1,
                            1,
                            1,
                            1,
                            1,
                            1,
                            1,
                            1,
                            1,
                            1,
                            1,
                            1

; The velocity correction is the distance from the center of the wire divided
; by the velocity of propagation times sdc_drifttime_sign(pln).  +/-1
; for disc. card at +/- coord. (i.e. top = -x direction, so top readout is +1)
;
; THESE ARE NOTHING BUT BAD GUESSES AT THE MOMENT!!!!
;
;For HKS DC, all should be subtracted.       L.Yuan  06/19/03
    hdc_drifttime_sign =  1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1

; Names of each wire plane
;   hdc_layer_name =  'hdc1u1',
;                     'hdc1u2',
;                     'hdc1x1',
;                     'hdc1x2',
;                     'hdc1v1',
;                     'hdc1v2',
;                     'hdc2u1',
;                     'hdc2u2',
;                     'hdc2x1',
;                     'hdc2x2',
;                     'hdc2v1',
;                     'hdc2v2'

; Add by L.Yuan for HKS 06/17/03, HKS DC X and Y active range (48.2in and 12 in)

    hdc_length_x = 122.428   
    hdc_length_y = 30.48     

    hdc_wire_offset_low = -4, -5, -3, -3, -4, -5,
                          -4, -5, -3, -3, -4, -5

    hdc_wire_offset_high = -5, -5, -4, -4, -5, -5, 
                           -5, -5, -4, -4, -5, -5

