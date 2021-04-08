CREATE OR REPLACE PACKAGE SISGODBA.PKG_RECAUDACIONBANCO IS
-- Log de cambios
-- 05.06.2019 - Juan Salazar C. - Creacion de package.
-- 27.08.2019 - Juan Salazar C. - Crecaion de Procedure P_GEN_PROCESAPAGOS, P_GEN_CANCELACUOTA
-- 11.11.2019 - Juan Salazar C. - Se agrega parametro PIFechaPagoBoleta para generar Boleta con Fecha de Pago Retro
-- 03.01.2020 - David Chara I. - Se agrego los metodos P_ACT_ACTUALIZACAJERO y P_GRABAPAGOPRESTAMOS y se agrego el parametro PIFechaProceso en la lectura del txt de recaudacion
-- 21.01.2020 - David Chara I. - Se modifico la obtencion de la fecha pago y el codigo de socio en la carga del txt
-- 10.03.2020 - David Chara I. - Ajustes del capital cuando la cuota es atrasada
-- 19.04.2020 - David Chara I. - Se modifico el metodo P_GEN_CANCELACUOTA
-- 20.05.2020 - Juan Salazar C. - Se crearon las funciones F_OBT_SALDOPORTES, F_OBT_SALDOREAJUSTE, F_OBT_SALDOSEGUROINTERES 
-- 01.06.2020 - Juan Salazar C. - Se agregaron tres parametros en el procedimiento P_GEN_CANCELACUOTA
--                                PIPeriodosolicitud, PINumerosolicitud, PITipopago, PINumerocuota
-- 03.12.2020 - Kenji Jhoncon -  Se agrega P_GEN_CARGAGLOBOKAS y P_GEN_CARGAINTERBANK. Ajuste P_GEN_CANCELACUOTA y P_GEN_PAGOPRESTAMOS por redondeo globokas, diferencia queda en AHV.
-- 09.03.2021 - Kenji Jhoncon -  Correccion en P_GEN_CARGAGLOBOKAS, segmentaba mal la trama.
-- 07.04.2021 - Kenji Jhoncon -  Se agrega P_GEN_CARGABANCONACION.
-- 
PROCEDURE P_GEN_CARGABANCONACION( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
			PIFechaProceso IN DATE:=SYSDATE
                          );
--
PROCEDURE P_GEN_CARGAINTERBANK( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
			PIFechaProceso IN DATE:=SYSDATE
                          );
--
PROCEDURE P_GEN_CARGAGLOBOKAS( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
			PIFechaProceso IN DATE:=SYSDATE
                          );
--
PROCEDURE P_GEN_CARGACONTI( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
			PIFechaProceso IN DATE:=SYSDATE
                          );
--
PROCEDURE P_GEN_CARGAPICHINCHA( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
			PIFechaProceso IN DATE:=SYSDATE
                          );
--
PROCEDURE P_GEN_CARGACREDITO( PIDirectorio    VARCHAR2,
                              PINombreArchivo VARCHAR2,
			      PIFechaProceso IN DATE:=SYSDATE			
                            );
--
PROCEDURE P_GEN_CARGASCOTIABANK( PIDirectorio    VARCHAR2,
                                 PINombreArchivo VARCHAR2,
			PIFechaProceso IN DATE:=SYSDATE
                               );
--
PROCEDURE P_GEN_PROCESAPAGOS( PICodigopersona     IN persona.codigopersona%TYPE,
                              PICuentaBanco       IN datosbanco.cuentabanco%TYPE,
                              PIMonedaCuenta      IN cajadetalle.moneda%TYPE,
                              PITipocambio        IN cajadetallerubros.tipocambio%TYPE,
                              PINumeroCheque      IN caja.numerocheque%TYPE,
                              PIImporte           IN prestamo.saldoprestamo%TYPE,
                              PIPeriodosolicitud  IN prestamo.periodosolicitud%TYPE,
                              PINumerosolicitud   IN prestamo.numerosolicitud%TYPE,
                              PIFechaCarga   IN DATE,
                              POestadotx          OUT VARCHAR2,
                              POidoperacion       OUT NUMBER,
                              PONumeroCtaSocio    OUT NUMBER,
                              POmensaje           OUT VARCHAR2,
                              PIFechaBoleta   IN DATE,                              
                              PIFechaEnvio IN DATE      
                            ) ;
--
PROCEDURE P_GEN_CANCELACUOTA( PITipoProceso IN NUMBER,  -- 1 Procesa Boleta Sin Pagar prestamo
                              POmensaje     OUT VARCHAR2,  -- 2 Procesa Boleta con Pago prestamo
                              PIPeriodoSolicitud IN NUMBER DEFAULT NULL,
                              PINumeroSolicitud IN NUMBER DEFAULT NULL,
                              PITipopago        IN VARCHAR2 DEFAULT NULL,
                              PINumerocuota     IN NUMBER DEFAULT NULL
                            );
--
PROCEDURE P_GEN_PAGOPRESTAMOS ( PICodigopersona        IN persona.codigopersona%TYPE,
                                PINumerocuenta         IN cuentacorriente.numerocuenta%TYPE,
                                PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                                PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                                PIImporteOrigen        IN prestamo.saldoprestamo%TYPE,  --Kenji 2020-12-03 Por Redondeo Globokas
                                PIImporte              IN prestamo.saldoprestamo%TYPE,
                                PIPagarCapital         IN prestamocuotas.amortizacion%TYPE,
                                PIInteres              IN prestamocuotas.interes%TYPE,
                                PIMora                 IN prestamo.moranocobrado%TYPE,
                                PIDesgravamen          IN prestamocuotas.reajuste%TYPE,
                                PIGastosCobranzas      IN prestamo.gastos%TYPE,
                                PIAporte               IN prestamocuotas.portes%TYPE,
                                PISeguroBien           IN prestamocuotas.segurointeres%TYPE,
                                --PIFechaCalculo         IN prestamo.fechaprestamo%TYPE,
                                PIComentario           IN caja.glosa%TYPE,
                                PITipoCambio           IN cajadetallerubros.tipocambio%TYPE,
                                PIFechaCarga           IN recaudacionbanco.fechacarga%TYPE,
                                --
                                POestadotx          OUT VARCHAR2,
                                POidoperacion       OUT NUMBER,
                                POmontoadebitar     OUT NUMBER,
                                POmontoadepositar   OUT NUMBER,
                                POmensaje           OUT VARCHAR2,--,
                                PIFechaEnvio        IN DATE    -- 03/02/2020 - David Chara I.
                                --PIFechaProceso      IN DATE :=SYSDATE
                              );
--
FUNCTION F_OBT_SALDOINTERES( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                             PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                             PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                              ) RETURN NUMBER;

--
FUNCTION F_OBT_SALDOMORA( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                          PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                          PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                              ) RETURN NUMBER;
--


PROCEDURE P_GRABAPAGOPRESTAMOS ( PICodigoagenciacaja    IN caja.codigoagenciacaja%TYPE,
                                 PIPeriodocaja          IN caja.periodocaja%TYPE,
                                 PINumerocaja           IN caja.numerocaja%TYPE,
                                 PIMoneda               IN cajadetalle.moneda%TYPE,
                                 PICodigopersona        IN persona.codigopersona%TYPE,
                                 PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                                 PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                                 PIImporte              IN prestamo.saldoprestamo%TYPE,
                                 PIPagarCapital         IN prestamocuotas.amortizacion%TYPE,  -- Total Amortizacion
                                 PIInteres              IN prestamocuotas.interes%TYPE,       -- Total Interes
                                 PIMora                 IN prestamo.moranocobrado%TYPE,       -- Total Mora
                                 PIDesgravamen          IN prestamocuotas.reajuste%TYPE,      -- Total Desgravamen
                                 PIGastosCobranzas      IN prestamo.gastos%TYPE,              -- Total Gastos
                                 PIAporte               IN prestamocuotas.portes%TYPE,        -- Total Aportes
                                 PISeguroBien           IN prestamocuotas.segurointeres%TYPE, -- Total Seguro
                                 PIFechaEnvio           IN DATE, 
                                 --PIFechaCalculo         IN prestamo.fechaprestamo%TYPE,      -- Fecha Proceso
                                 PIFormaPago            IN caja.formapago%TYPE,              -- Forma Pago
                                 PITransaccion          IN cajadetalle.grupomovimiento%TYPE,
                                 PIComentario           IN caja.glosa%TYPE,
                                 PICodigoPersonaDestino IN persona.codigopersona%TYPE,
                                 PIDestino              IN cajadetalle.numerodocumento%TYPE,
                                 PIBancoOrigen          IN syst900.tblcodarg%TYPE,
                                 PITotal                IN caja.importe%TYPE,
                                 PIGastoTrans           IN caja.importe%TYPE,
                                 PIGastoAdm             IN caja.importe%TYPE,
                                 PIMonedaPrestamo       IN cajadetalle.moneda%TYPE,
                                 PITipocambio           IN cajadetallerubros.tipocambio%TYPE,
                                 PIFechaCarga         IN DATE := SYSDATE
                                );
                                
PROCEDURE P_ACT_ACTUALIZACAJERO ( pOperacion VARCHAR2,
                                  pImporte   NUMBER,
                                  pMoneda    NUMBER,
                                  pAgencia   NUMBER,
                                  pFecha     DATE,
                                  vMensaje Out Varchar2 );
                                  
                                  
PROCEDURE CRE07132( P_PeriodoSolicitud  IN NUMBER,
                                      P_NumeroSolicitud   IN NUMBER,
                                      P_FechaMovimiento   IN DATE,
                                      P_TipoMovimiento    IN NUMBER,
                                      P_Capital           IN NUMBER,
                                      P_Interes           IN NUMBER,
                                      P_InteresNoCobrado  IN NUMBER,
                                      P_Mora              IN NUMBER,
                                      P_MoraNoCobrado     IN NUMBER,
                                      P_Condicion         IN NUMBER,
                                      P_Glosa             IN VARCHAR2,
                                      P_Desgravamen       IN NUMBER,
                                      P_Observaciones     IN VARCHAR2,
                                      P_TransferenciaGarantia IN INTEGER,
                                      P_FechaDeposito     IN DATE,
                                      P_CodigoAgencia     IN NUMBER,
                                      P_FormaPago         IN NUMBER,
                                      P_Portes            IN NUMBER,
                                      P_SeguroBien        IN NUMBER,
                                      P_CODIGOAGENCIACAJA IN NUMBER,
                                      P_PERIODOCAJA       IN NUMBER,
                                      P_NUMEROCAJA        IN NUMBER,
                                      P_NumeroItem        OUT NUMBER
                                    );
                                    
                                    
PROCEDURE P_OBT_VERIFICARDEBITOAUTO( pPeriodoSolicitud NUMBER,
                                     pNumeroSolicitud   NUMBER,                                  
                                     vResultado OUT NUMBER );
--
FUNCTION F_OBT_SALDOPORTES( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                            PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                            PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                          ) RETURN NUMBER;
--
FUNCTION F_OBT_SALDOREAJUSTE( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                              PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                              PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                            ) RETURN NUMBER;
--
FUNCTION F_OBT_SALDOSEGUROINTERES( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                                   PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                                   PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                                 ) RETURN NUMBER;
--   
END PKG_RECAUDACIONBANCO;
/