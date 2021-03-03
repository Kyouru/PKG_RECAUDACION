CREATE OR REPLACE PACKAGE SISGODBA.PKG_RECAUDACIONENVIO IS
-- Log de cambios
-- 13.09.2019 - Gudalupe Monar  - Ajustes en P_GEN_DATOSRECAUDACION
-- 16.09.2019 - Juan Salazar C. - Creaci?n de procedure P_GEN_ARCHIVOTXT
-- 16.09.2019 - Bryan Ordo?ez M. - Ajuste en F_GET_MONTOADEUDADO
-- 11.11.2019 - David Chara I. - Ajuste P_GEN_ARCHIVOTXT y P_GEN_DATOSRECAUDACION para incluir al banco Scotiabank
-- 18.11.2019 - David Chara I. - Ajuste P_GEN_DATOSRECAUDACION para incluir la funcion F_GEN_CADENALIMPIA en los nombres de los socios
-- 10.01.2020 - David Chara I. - Ajuste en los caracteres del numero de solicitud P_GEN_DATOSRECAUDACION
-- 25.11.2020 - Kenji Jhoncon - Agregar Recaudacion Interbank y GloboKasnet. Ajustes en P_GEN_DATOSRECAUDACION, P_GEN_ARCHIVOTXT, P_GEN_GENERARARCHIVOS
-- 26.11.2020 - Kenji Jhoncon - Creacion de procedure P_GEN_ARCHIVOTXT_IBK, para poder crear un unico archivo .txt con recaudacion de ambas monedas. Ajustes en P_GEN_DATOSRECAUDACION, P_GEN_ARCHIVOTXT, P_GEN_GENERARARCHIVOS
-- 25.02.2021 - Kenji Jhoncon - Ajuste P_GEN_ARCHIVOTXT, numero servicio Kasnet completar con ceros a la izquierda

FUNCTION F_GET_MONTOADEUDADO ( P_PeriodoSolicitud IN NUMBER,
                               P_NumeroSolicitud  IN NUMBER,
                               PIValorColumna     IN NUMBER  ) RETURN NUMBER;
--
FUNCTION F_GET_VALRECAUDAC ( P_PeriodoSolicitud Number, P_NumeroSolicitud Number)
 RETURN VARCHAR2 ;
--
FUNCTION GENERATXT ( P_NumeroCuenta IN NUMBER,
                     P_FechaDesde   IN DATE,
                     P_FechaHasta   IN DATE,
                     P_Opcion       IN NUMBER := 0 ) RETURN tMovimientosCaptacion PIPELINED;
--
FUNCTION F_OBT_FECVENCADEUDADO ( PIPeriodoSolicitud IN NUMBER,
                                 PINumeroSolicitud  IN NUMBER ) RETURN DATE;
--
PROCEDURE P_GEN_DATOSRECAUDACION (PIFECHA IN DATE,
                                  PIMONEDA IN NUMBER,
                                  POERROR OUT NUMBER);
--
FUNCTION F_OBT_INTERESATRASADO (PIPeriodoSolicitud IN NUMBER,
                                PINumeroSolicitud  IN NUMBER,
                                PIFecha IN Date ) Return Number;
--
PROCEDURE P_GEN_ARCHIVOTXT( PICodigoBanco datosbanco.codigobanco%TYPE,
                            PIMonedaBanco syst900.tblcodtab%TYPE
                          );
--
PROCEDURE P_GEN_GENERARARCHIVOS (PIFECHA IN DATE);
--
PROCEDURE P_GEN_ARCHIVOTXT_IBK;
--
END PKG_RECAUDACIONENVIO;
/