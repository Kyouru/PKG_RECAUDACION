CREATE OR REPLACE PACKAGE BODY SISGODBA.PKG_RECAUDACIONENVIO Is
    -- Log de cambios 
    -- 13.09.2019 - Gudalupe Monar  - Ajustes en P_GEN_DATOSRECAUDACION
    -- 16.09.2019 - Juan Salazar C. - Creaci?n de procedure P_GEN_ARCHIVOTXT
    -- 16.09.2019 - Bryan Ordo?ez M.- Ajuste en F_GET_MONTOADEUDADO
    -- 11.11.2019 - David Chara I. - Ajuste P_GEN_ARCHIVOTXT y P_GEN_DATOSRECAUDACION para incluir al banco Scotiabank
    -- 18.11.2019 - David Chara I. - Ajuste P_GEN_DATOSRECAUDACION para incluir la funcion F_GEN_CADENALIMPIA en los nombres de los socios
    -- 16.01.2020 - David Chara I. - Se agrego la insercion de valores a la tabla RECAUDACIONENVIO de la data enviada al generar los txt de recaudacion
    -- 25.11.2020 - Kenji Jhoncon - Agregar Recaudacion Interbank y GloboKasnet. Ajustes en P_GEN_DATOSRECAUDACION, P_GEN_ARCHIVOTXT, P_GEN_GENERARARCHIVOS
    -- 26.11.2020 - Kenji Jhoncon - Creacion de procedure P_GEN_ARCHIVOTXT_IBK, para poder crear un unico archivo .txt con recaudacion de ambas monedas. Ajustes en P_GEN_DATOSRECAUDACION, P_GEN_ARCHIVOTXT, P_GEN_GENERARARCHIVOS
    -- 25.02.2021 - Kenji Jhoncon - Ajuste P_GEN_ARCHIVOTXT, numero servicio Kasnet completar con ceros a la izquierda

    FUNCTION F_GET_MONTOADEUDADO ( P_PeriodoSolicitud IN NUMBER,
                                   P_NumeroSolicitud  IN NUMBER,
                                   PIValorColumna     IN NUMBER  ) RETURN NUMBER IS

     -- Objetivo     : function que devuelve el monto adeudado por prestamo
     -- Responsable  : GCABALLERO
     -- Fecha        : 01/12/2017
      nMontoAdeudado number;
      PN_DEUDA_VENCIDA number;
      nAdicionales number;
      nDescProducto varchar2(100);
    BEGIN
        IF PIValorColumna = 1 THEN  -- Monto total
           SELECT SUM(SALDOCAPITAL)+
                  SUM(SALDOINTERES)+
                  SUM(SALDOMORA)
             INTO PN_DEUDA_VENCIDA
             FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY;
           --SISGODBA
           IF PN_DEUDA_VENCIDA = 0 THEN
              SELECT CAPITAL + INTERES +SEGUROINTERES+PORTES+REAJUSTE
                INTO nMontoAdeudado
                FROM TABLE(Cre08070.DeudacuotasSIP(P_PeriodoSolicitud,
                                         P_NumeroSolicitud,
                                         TO_DATE(TO_CHAR(HOY,
                                                        'ddmmyyyy'),
                                                        'ddmmyyyy')))
               WHERE rownum = 1
               ORDER BY fechavencimiento;
           ELSE
                -- ADICIONALES
                SELECT SUM(SEGUROINTERES)+ SUM(PORTES)+ SUM(REAJUSTE)
                  INTO nAdicionales
                  FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY))
                 WHERE FECHAVENCIMIENTO <= HOY
                   AND SALDOCAPITAL>0;
                 nMontoAdeudado:= PN_DEUDA_VENCIDA+nAdicionales;
           END IF;
           --
    --       RETURN nMontoAdeudado;
       ELSIF PIValorColumna = 2 THEN  -- SaldoCapital
             SELECT SUM(SALDOCAPITAL)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY
                AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- GCABALLERO 21/02/2018 Se a?ade la validacion para que no considere los saldos en cero
             --
             nMontoAdeudado:= PN_DEUDA_VENCIDA;
       ELSIF PIValorColumna = 3 THEN  -- SaldoInteres
             SELECT SUM(SALDOINTERES)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY
                AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- GCABALLERO 21/02/2018 Se a?ade la validacion para que no considere los saldos en cero
             --
             nMontoAdeudado:= PN_DEUDA_VENCIDA;
       ELSIF PIValorColumna = 4 THEN  -- SaldoMora
             --<INI 16.09.2019 Bryan ordo?ez M. para considerar el saldo de la mora para techo propio>--
             nDescProducto := PKG_SYST902.F_OBT_TBLDESCRI(PKG_PRESTAMODETALLE.F_OBT_TIPOSOLICITUD(P_PERIODOSOLICITUD, P_NUMEROSOLICITUD,0),
                                                          PKG_PRESTAMODETALLE.F_OBT_TIPOPRESTAMO(P_PERIODOSOLICITUD, P_NUMEROSOLICITUD, 0));

             IF SUBSTR(nDescProducto,1,3) = 'PCT' THEN
              SELECT SUM(SALDOMORA)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( PKG_TECHO_PROPIO.F_OBT_DEUDAPCT(P_PeriodoSolicitud, P_NumeroSolicitud, HOY))
                WHERE (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
             ELSE
             --<FIN 16.09.2019 Bryan ordo?ez M.> --
              SELECT SUM(SALDOMORA)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY
                AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- GCABALLERO 21/02/2018 Se a?ade la validaci?n para que no considere los saldos en cero
             END IF;
             --
             nMontoAdeudado:= PN_DEUDA_VENCIDA;
       ELSIF PIValorColumna = 5 THEN  -- SeguroInteres
             SELECT SUM(SEGUROINTERES)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY
                  AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- GCABALLERO 21/02/2018 Se a?ade la validacion para que no considere los saldos en cero

             nMontoAdeudado:= PN_DEUDA_VENCIDA;
       ELSIF PIValorColumna = 6 THEN  -- Portes
             SELECT SUM(PORTES)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY
               AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- GCABALLERO 21/02/2018 Se a?ade la validacion para que no considere los saldos en cero

             --
             nMontoAdeudado:= PN_DEUDA_VENCIDA;
       ELSIF PIValorColumna = 7 THEN  -- REAJUSTE
             SELECT SUM(REAJUSTE)
               INTO PN_DEUDA_VENCIDA
               FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY
               AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- GCABALLERO 21/02/2018 Se a?ade la validacion para que no considere los saldos en cero
             --
             nMontoAdeudado:= PN_DEUDA_VENCIDA;
       END IF;
       RETURN nMontoAdeudado;
    EXCEPTION WHEN OTHERS THEN
              nMontoAdeudado :=0;
              RETURN nMontoAdeudado;
    END F_GET_MONTOADEUDADO;
    --
    FUNCTION F_GET_VALRECAUDAC (P_PeriodoSolicitud Number, P_NumeroSolicitud Number)
     RETURN VARCHAR2 IS

     -- Objetivo     : function que devuelve valores para recaudacion
     -- Responsable  : GCABALLERO
     -- Fecha        : 01/12/2017
     --nMontoAdeudado number;
      PN_DEUDA_VENCIDA number;
      nAdicionales number;
      nMora number;
      vNumeroCuota varchar2(1);
      vVencido varchar2(3);
      vFecVencimiento varchar2(8);
      vFecVencimiento2 varchar2(8);
      vResultado varchar2(100);
    BEGIN
    -- Si tiene deuda vencida:

    SELECT SUM(SALDOCAPITAL)+
           SUM(SALDOINTERES)+
           SUM(SALDOMORA),SUM(SALDOMORA)
      INTO PN_DEUDA_VENCIDA, nMora
      FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY;

        -- NO TIENE DEUDA VENCIDA
        IF PN_DEUDA_VENCIDA = 0 THEN
            vVencido := 'ACT';
          BEGIN
           SELECT TO_CHAR(TO_DATE(FECHAVENCIMIENTO,'DD/MM/RRRR'),'RRRRMMDD'), TO_CHAR(TO_DATE(FECHAVENCIMIENTO,'DD/MM/RRRR')+1,'RRRRMMDD'),numerocuota
             INTO vFecVencimiento, vFecVencimiento2 , vNumeroCuota
             FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY))
            WHERE FECHAVENCIMIENTO >= HOY
              AND ROWNUM = 1
            ORDER BY FECHAVENCIMIENTO DESC;

            nMora := 0;
          EXCEPTION WHEN OTHERS THEN
             DBMS_OUTPUT.PUT_LINE('Error ACT'||p_periodosolicitud||'-'||p_numerosolicitud||' -'||SQLERRM);
          END;

        ELSE
         vVencido:= 'ATR';
        BEGIN
         SELECT TO_CHAR(TO_DATE(FECHAVENCIMIENTO,'DD/MM/RRRR'),'RRRRMMDD'), TO_CHAR(TO_DATE(FECHAVENCIMIENTO,'DD/MM/RRRR')+1,'RRRRMMDD'), NumeroCuota
           INTO vFecVencimiento, vFecVencimiento2, vNumeroCuota
           FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY))
          WHERE FECHAVENCIMIENTO <= HOY
            and SALDOCAPITAL >0
            AND ROWNUM = 1
          ORDER BY FECHAVENCIMIENTO DESC;
        EXCEPTION WHEN OTHERS THEN
             DBMS_OUTPUT.PUT_LINE('Error ATR'||p_periodosolicitud||'-'||p_numerosolicitud||' -'||SQLERRM);
        END;
        -- ADICIONALES
        SELECT SUM(SEGUROINTERES)+ SUM(PORTES)+ SUM(REAJUSTE)
          INTO nAdicionales
          FROM TABLE( CRE08070.DEUDACUOTASSIP(P_PeriodoSolicitud, P_NumeroSolicitud, HOY))
         WHERE FECHAVENCIMIENTO <= HOY
           AND SALDOCAPITAL>0;

        -- nMontoAdeudado:= PN_DEUDA_VENCIDA+nAdicionales;
        END IF;
        vResultado:= vVencido||'@'||vFecVencimiento||'@'||vFecVencimiento2||'@'||vNumeroCuota||'@'||nMora;
        RETURN vResultado;

    END F_GET_VALRECAUDAC ;
    --
    FUNCTION GENERATXT ( P_NumeroCuenta In Number,
                         P_FechaDesde In Date,
                         P_FechaHasta In Date,
                         P_Opcion In Number := 0 ) Return tMovimientosCaptacion Pipelined Is

    mc oMovimientosCaptacion := oMovimientosCaptacion(Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null, Null);

    CURSOR Movimientos IS
    SELECT c.NombreCuenta, c.CodigoPersona, GEN05030(c.CodigoPersona) NombreCompleto,
            GEN05070(22, c.Moneda) Moneda, c.SaldoImporte1 SaldoContable,
            CAP05040(c.NumeroCuenta) SaldoDisponible, GEN05070(27, c.Estado) Estado,
            a.NumeroDocumento, a.FechaMovimiento, a.TipoMovimiento, GEN05070(15, a.TipoMovimiento) DescripcionTipoMovimiento,
            a.CodigoAgencia, a.Observacion, a.Importe1, a.SaldoImporte1, GEN05070(21, a.FormaPago) DescripcionFormaPago,
                LPAD(a.CodigoAgenciaCaja,3,0)||'-'||LPAD(a.PeriodoCaja,6,0)||'-'||LPAD(a.NumeroCaja,7,0) Recibo,
            a.CodigoUsuario, a.FechaUsuario
      FROM CuentaCorriente c, Aportes a
     WHERE a.NumeroCuenta = P_NumeroCuenta
         AND a.Estado = 1
        --and a.NumeroDocumento >= (Select Max(NumeroDocumento) - 10 From Aportes Where NumeroCuenta = :P_NumeroCuenta)
         AND ((P_Opcion = 1 AND a.NumeroDocumento >= ( SELECT MAX(NumeroDocumento) - 10
                                                       FROM Aportes
                                                      WHERE NumeroCuenta = P_NumeroCuenta))
                      OR (P_Opcion = 2 AND TRUNC(FechaMovimiento) BETWEEN P_FechaDesde AND P_FechaHasta)
                      OR (P_Opcion IN (0, 3)))
       AND c.NumeroCuenta = a.NumeroCuenta
     ORDER BY a.NumeroDocumento;

     m Movimientos%RowType;
    BEGIN
         FOR m IN Movimientos LOOP
               mc.NombreCuenta := m.NombreCuenta;
             mc.CodigoPersona := m.CodigoPersona;
             mc.NombreCompleto := m.NombreCompleto;
             mc.Moneda := m.Moneda;
             mc.SaldoContable := m.SaldoContable;
             mc.SaldoDisponible := m.SaldoDisponible;
             mc.Estado := m.Estado;
               mc.NumeroDocumento := m.NumeroDocumento;
             mc.FechaMovimiento := m.FechaMovimiento;
             mc.TipoMovimiento := m.TipoMovimiento;
             mc.DescripcionTipoMovimiento := m.DescripcionTipoMovimiento;
             mc.CodigoAgencia := m.CodigoAgencia;
             mc.Observacion := m.Observacion;
             mc.Importe1 := m.Importe1;
             mc.SaldoImporte1 := m.SaldoImporte1;
             mc.DescripcionFormaPago := m.DescripcionFormaPago;
             mc.Recibo := m.Recibo;
             mc.CodigoUsuario := m.CodigoUsuario;
             mc.FechaUsuario := m.FechaUsuario;

               Pipe Row(mc);
         END LOOP;
         Return;
    END GENERATXT;
    --
    FUNCTION F_OBT_FECVENCADEUDADO ( PIPeriodoSolicitud IN NUMBER,
                                     PINumeroSolicitud  IN NUMBER ) RETURN DATE IS
     PN_DEUDA_VENCIDA number;
     vFechaVencimiento DATE;

    BEGIN
           SELECT SUM(SALDOCAPITAL)+
                  SUM(SALDOINTERES)+
                  SUM(SALDOMORA)
             INTO PN_DEUDA_VENCIDA
             FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodoSolicitud, PINumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY;
           --
           IF PN_DEUDA_VENCIDA = 0 THEN
              vFechavencimiento := HOY;
           ELSE
               SELECT MAX(fechavencimiento)
                 INTO vFechavencimiento
                 FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodoSolicitud, PINumeroSolicitud, HOY)) WHERE FECHAVENCIMIENTO <= HOY;
           END IF;
           RETURN vFechavencimiento;
    END F_OBT_FECVENCADEUDADO;
--
PROCEDURE P_GEN_DATOSRECAUDACION( PIFECHA  IN DATE,
                                  PIMONEDA IN NUMBER,
                                  POERROR  OUT NUMBER
                                ) IS
    CURSOR detalle IS
    SELECT 1 AS COD1,
           2 AS COD2,
           '02' AS TIP_REGISTRO,
           RPAD(TRIM(SUBSTR(pkg_persona.f_obt_nombrecompletobancos(pre.Codigopersona ),1,23)),23,' ')
           ||(SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( cre08070.deudacuotassip(pre.periodosolicitud, pre.numerosolicitud, hoy))
               WHERE fechavencimiento <= HOY)
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           (SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
               WHERE FECHAVENCIMIENTO <= HOY)||TO_CHAR(HOY,'MMDD')AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(substr(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',
                  PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                  SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'|| LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                 SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||'ATR',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') ||  
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''|| 
            SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
            LPAD(pre.PeriodoSolicitud,4,0)||''||
            LPAD(pre.NumeroSolicitud,7,0)||
            --pre.NumeroSolicitud||
           'ATR')PAGO_ID,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),'PTP',
           (SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
               WHERE FECHAVENCIMIENTO <= HOY)||TO_CHAR(HOY,'MMDD')           
           ||'-'||
           SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'|| LPAD(SP.PERIODOSOLICITUD,4,0) ||'-'||
           SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||'ATR',
           --per.cip||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           LPAD(pre.NumeroSolicitud,7,0)||
           'ATR'  
           /*|| NVL((SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
               WHERE FECHAVENCIMIENTO <= HOY),'001')*/
           ) PAGO_ID_2,
           DECODE(substr(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',
                  PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                  SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'|| LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                 SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||'ATR',
           LPAD(per.cip,7,0) ||''||             
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''|| 
            SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
            LPAD(pre.PeriodoSolicitud,4,0)||''||
            LPAD(pre.NumeroSolicitud,7,0)||           
           'ATR' ||
           TO_CHAR(HOY,'YYMMDD'))PAGO_ID_CONTINENTAL,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 1 ) AS Monto_Minimo,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 2 ) AS SALDOCAPITAL,
           CASE WHEN SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)='PTP'
                 AND PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 2 )=0  THEN
                 PKG_RECAUDACIONENVIO.F_OBT_INTERESATRASADO (pre.PeriodoSolicitud,pre.NumeroSolicitud,hoy)
           ELSE
               PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 3 )
           END AS SALDOINTERES,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 4 ) AS SALDOMORA,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 5 ) AS SEGUROINTERES,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 6 ) AS APORTES,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 7 ) AS REAJUSTE,
           NVL(PKG_RECAUDACIONENVIO.F_OBT_FECVENCADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud ), trunc(HOY) ) AS FECHAVENCIMIENTO,
           pkg_cartera.DIAS_ATRASO_CARTERA(PIFECHA ,pre.PeriodoSolicitud , pre.NumeroSolicitud ) DIASATRASO,
           pre.codigopersona,
           (SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
              FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
             WHERE FECHAVENCIMIENTO <= HOY) AS NUMEROCUOTA,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd
            ON pd.periodosolicitud = presdet.periodosolicitud
           AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc FROM persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn
                  INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON  soc.codigopersona = pre.codigopersona

     WHERE pre.moneda = PIMONEDA
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND substr(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3) NOT  IN ('PCC','PCY','PCM','PFI',-- CARTERA
                                                                                            'PDP', 'PDD','PLR','PLC', 'TAN'-- Descuento por Planilla
                                                                                              )
       AND PRE.PERIODOSOLICITUD<>1        
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                     AND numerosolicitudconcesional IS NOT NULL)
       AND NVL(DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),'PTP',
              ( SELECT MAX(numerocuota)
                  FROM TABLE( CRE08070.DEUDACUOTASSIP(pre.periodosolicitud, pre.numerosolicitud, HOY))
                 WHERE fechavencimiento <= HOY ),pkg_cartera.DIAS_ATRASO_CARTERA(PIFECHA ,pre.PeriodoSolicitud , pre.NumeroSolicitud )),0)>0
       AND pre.periodosolicitudconcesional IS NULL AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN(SELECT periodosolicitud, numerosolicitud
                                                                 FROM solicitudprestamo
                                                                 WHERE periodosolicitudconcesional IS NOT NULL
                                                                 AND numerosolicitudconcesional IS NOT NULL )
     UNION ALL
    SELECT 1 AS cod1,
           2 AS cod2,
           '02' TIP_REGISTRO,
           RPAD(TRIM(SUBSTR( PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(pre.codigopersona ),1,23)),23,' ')
           ||LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')||
           TO_CHAR(HOY,'MMDD') AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           LPAD(pre.NumeroSolicitud,7,0)||
           --pre.NumeroSolicitud||-- PAGO_ID,
           'ACT') PAGO_ID ,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                            FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, 
                                                                PRE.NumeroSolicitud, HOY)
                                      )
                           WHERE FECHAVENCIMIENTO <= HOY )||
                        TO_CHAR(HOY,'MMDD')||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
                  --per.cip ||''||
                  TO_CHAR(HOY,'YYMMDD') ||
                  SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
                  SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
                  LPAD(pre.PeriodoSolicitud,4,0)||''||
                  LPAD(pre.NumeroSolicitud,7,0)||
                  'ACT'
                  /*|| 
                  NVL(( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                      FROM TABLE( CRE08070.DEUDACUOTASSIP( PRE.PeriodoSolicitud, 
                                                           PRE.NumeroSolicitud, 
                                                           HOY 
                                                         )
                  )
                     WHERE FECHAVENCIMIENTO <= HOY),'001')*/           
            )PAGO_ID_2,    
            DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           LPAD(per.cip,7,0) ||''||                       
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           LPAD(pre.NumeroSolicitud,7,0)||            
           'ACT' ||
           TO_CHAR(HOY,'YYMMDD')) PAGO_ID_CONTINENTAL,                 
           pc.AMORTIZACION + pc.INTERES as Monto_MINIMO,
           pc.AMORTIZACION AS SALDOCAPITAL,
           pc.INTERES AS SALDOINTERES,
           0 AS SALDOMORA,
           NVL(pc.segurointeres,0) AS SEGUROINTERES,
           NVL(pc.portes,0) AS APORTES,
           NVL(pc.reajuste,0) AS REAJUSTE,
           NVL(pc.fechavencimiento,TRUNC(HOY)) AS FECHAVENCIMIENTO,
           0 DIASATRASO,
           pre.codigopersona,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0') AS numerocuota,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd ON pd.periodosolicitud = presdet.periodosolicitud AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN prestamocuotas pc ON pre.periodosolicitud = pc.periodosolicitud AND pre.numerosolicitud  = pc.numerosolicitud
    INNER JOIN  ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc from persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON soc.codigopersona = pre.codigopersona

     WHERE pc.estado = 2
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.moneda =PIMONEDA
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)   NOT  IN ('PCC','PCY','PCM','PFI',-- CARTERA
                                                                                                    'PDP', 'PDD','PLR','PLC', 'TAN'-- Descuento por Planilla
                                                                                                   )
       AND PRE.PERIODOSOLICITUD <> 1       
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                    AND numerosolicitudconcesional IS NOT NULL)
       AND TRUNC(pc.fechavencimiento) IN (SELECT MIN(fechavencimiento ) FROM prestamocuotas
                                             WHERE periodosolicitud = pre.periodosolicitud
                                               AND numerosolicitud = pre.numerosolicitud
                                               AND estado =2
                                               AND fechavencimiento>= HOY
                                            )
     ---Pago de Inscripcion en Cuotas
       UNION ALL                                     
       SELECT 1 AS cod1,
           2 AS cod2,
           '02' TIP_REGISTRO,
           RPAD(TRIM(SUBSTR( PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(pre.codigopersona ),1,23)),23,' ')
           ||LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')||
           TO_CHAR(HOY,'MMDD') AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           --pre.NumeroSolicitud||-- PAGO_ID,
           LPAD(pre.NumeroSolicitud,7,0) ||
           'ACT') PAGO_ID ,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                            FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, 
                                                                PRE.NumeroSolicitud, HOY)
                                      )
                           WHERE FECHAVENCIMIENTO <= HOY )||
                        TO_CHAR(HOY,'MMDD')||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
                  --per.cip ||''||
                  TO_CHAR(HOY,'YYMMDD') || 
                  SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
                  SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
                  LPAD(pre.PeriodoSolicitud,4,0)||''||
                  LPAD(pre.NumeroSolicitud,7,0)||
                  'ACT'
                  /*|| 
                  NVL(( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                      FROM TABLE( CRE08070.DEUDACUOTASSIP( PRE.PeriodoSolicitud, 
                                                           PRE.NumeroSolicitud, 
                                                           HOY 
                                                         )
                  )
                     WHERE FECHAVENCIMIENTO <= HOY),'001')*/           
            )PAGO_ID_2,    
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           LPAD(per.cip,7,0) ||''||            
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||           
           LPAD(pre.NumeroSolicitud,7,0) ||
           'ACT'||
           TO_CHAR(HOY,'YYMMDD')
           ) PAGO_ID_CONTINENTAL ,                 
           pc.AMORTIZACION + pc.INTERES as Monto_MINIMO,
           pc.AMORTIZACION AS SALDOCAPITAL,
           pc.INTERES AS SALDOINTERES,
           0 AS SALDOMORA,
           NVL(pc.segurointeres,0) AS SEGUROINTERES,
           NVL(pc.portes,0) AS APORTES,
           NVL(pc.reajuste,0) AS REAJUSTE,
           NVL(pc.fechavencimiento,TRUNC(HOY)) AS FECHAVENCIMIENTO,
           0 DIASATRASO,
           pre.codigopersona,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0') AS numerocuota,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd ON pd.periodosolicitud = presdet.periodosolicitud AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN prestamocuotas pc ON pre.periodosolicitud = pc.periodosolicitud AND pre.numerosolicitud  = pc.numerosolicitud
    INNER JOIN  ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc from persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON soc.codigopersona = pre.codigopersona

     WHERE pc.estado = 2      
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.moneda =PIMONEDA
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND PRE.PERIODOSOLICITUD = 1
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                    AND numerosolicitudconcesional IS NOT NULL)
      --<04.03.2020-Richard Rodriguez -Para que traiga la primera cuota en estado vigente-Techo propio>                                                              
       /*AND TRUNC(pc.fechavencimiento) IN (SELECT fechavencimiento  FROM prestamocuotas
                                             WHERE periodosolicitud = pre.periodosolicitud
                                               AND numerosolicitud = pre.numerosolicitud
                                               AND estado =2
                                               AND fechavencimiento<= HOY
                                       )*/
        AND pc.numerocuota=1
         /*AND pc.numerocuota IN (SELECT MIN(numerocuota)  FROM prestamocuotas
                                             WHERE periodosolicitud = pre.periodosolicitud
                                               AND numerosolicitud = pre.numerosolicitud
                                               AND estado =2       
                                         )*/
        AND pc.estado=2
        AND sp.estado!=3
        --<F.04.03.2020-Richard Rodriguez -Para que traiga la primera cuota en estado vigente--Techo propio>   
        AND EXISTS (SELECT *
              FROM viviendaptp
              WHERE codigopersona = pre.codigopersona )
        AND EXISTS (    SELECT *
             FROM CuentaCorriente
            WHERE TipoTransaccion =2
              And Moneda = PIMoneda
              AND estado = 1
              AND CodigoPersona = pre.codigopersona
              AND tablaservicio=101
              AND argumentoservicio IN (13,14));

    /*UNION ALL
    
    SELECT 1 AS cod1,
           2 AS cod2,
           '02' TIP_REGISTRO,
           RPAD(TRIM(SUBSTR( PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(pre.codigopersona ),1,23)),23,' ')
           ||LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')||
           TO_CHAR(HOY,'MMDD') AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           --pre.NumeroSolicitud||-- PAGO_ID,
           LPAD(pre.NumeroSolicitud,7,0) ||
           'ACT') PAGO_ID ,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                            FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, 
                                                                PRE.NumeroSolicitud, HOY)
                                      )
                           WHERE FECHAVENCIMIENTO <= HOY )||
                        TO_CHAR(HOY,'MMDD')||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
                  --per.cip ||''||
                  TO_CHAR(HOY,'YYMMDD') || 
                  SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
                  SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
                  LPAD(pre.PeriodoSolicitud,4,0)||''||
                  LPAD(pre.NumeroSolicitud,7,0)||
                  'ACT'
               
            )PAGO_ID_2,    
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           LPAD(per.cip,7,0) ||''||            
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||           
           LPAD(pre.NumeroSolicitud,7,0) ||
           'ACT'||
           TO_CHAR(HOY,'YYMMDD')
           ) PAGO_ID_CONTINENTAL ,                 
           pc.AMORTIZACION + pc.INTERES as Monto_MINIMO,
           pc.AMORTIZACION AS SALDOCAPITAL,
           pc.INTERES AS SALDOINTERES,
           0 AS SALDOMORA,
           NVL(pc.segurointeres,0) AS SEGUROINTERES,
           NVL(pc.portes,0) AS APORTES,
           NVL(pc.reajuste,0) AS REAJUSTE,
           NVL(pc.fechavencimiento,TRUNC(HOY)) AS FECHAVENCIMIENTO,
           0 DIASATRASO,
           pre.codigopersona,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0') AS numerocuota,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd ON pd.periodosolicitud = presdet.periodosolicitud AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN prestamocuotas pc ON pre.periodosolicitud = pc.periodosolicitud AND pre.numerosolicitud  = pc.numerosolicitud
    INNER JOIN  ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc from persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON soc.codigopersona = pre.codigopersona

     WHERE pc.estado = 2      
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.moneda =PIMONEDA
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND PRE.PERIODOSOLICITUD = 1
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                    AND numerosolicitudconcesional IS NOT NULL)
       AND TRUNC(pc.fechavencimiento) IN (SELECT MIN(fechavencimiento ) FROM prestamocuotas
                                             WHERE periodosolicitud = pre.periodosolicitud
                                               AND numerosolicitud = pre.numerosolicitud
                                               AND estado =2
                                             AND fechavencimiento>= HOY
                                            )
        AND EXISTS (SELECT *
              FROM viviendaptp
              WHERE codigopersona = pre.codigopersona )
        AND EXISTS (    SELECT *
             FROM CuentaCorriente
            WHERE TipoTransaccion =2
              And Moneda = PIMoneda
              AND estado = 1
              AND CodigoPersona = pre.codigopersona
              AND tablaservicio=101
              AND argumentoservicio IN (13,14));*/
                                                                               
      vCabecera        VARCHAR2(360);
      vTipoRegistroCab VARCHAR2(2)  := '01';
      vRucEmpresa      VARCHAR2(11) := '20111065013';
      vNumeroClase     NUMBER(3)    := 500;
      vMoneda          VARCHAR2(3)  := 'PEN';
      vFechaFactura    VARCHAR2(8)  := TO_CHAR(HOY,'YYYYMMDD');
      vVersion         VARCHAR2(3)    := '000' ;
      vRellenoCab      VARCHAR2(330):= ' ';
      vDetalle         VARCHAR2(400):= ' ';
      vDetallefin      VARCHAR2(400):= ' ';
      vTipoRegistroDet VARCHAR2(2)  := '02';
      --vRellenoDet1      VARCHAR2(72) := '0';
     -- vRellenoDet2      VARCHAR2(72) := ' ';
      vPie             VARCHAR2(360):= ' ';
      vTipoRegistroPie VARCHAR2(2)  := '03';
      vRellenoPie1     VARCHAR2(18) := '0 ';
      vRellenoPie2     VARCHAR2(295):= ' ';
      vTotalAmortizacion NUMBER(15,2):=0;
      vMontoadeudado   NUMBER(15,2):=0;
      vTotalAdeudado   NUMBER(15,2):=0;
      vTotalMinimo     NUMBER(15,2):=0;
      vConteoTotal     NUMBER(9):=0;
      vTipoPersona     NUMBER(1);
      vNumeroDocumento VARCHAR2(15);
      vTipodocumento   VARCHAR2(1);
      vTipodocumentofin  VARCHAR2(1);
      --
      vCabeceraCREDITO   VARCHAR2(360);
      vTipoRegistroCabCREDITO VARCHAR2(2)  := 'CC';
      vMonedaCREDITO     VARCHAR2(3);
      vSucursalCREDITO   VARCHAR2(3) := '191';
      vCodigoAfiliacion  VARCHAR2(7) := ' ';
      vTipoValidaCREDITO VARCHAR2(1) := 'C';  -- C = Completa solo para BCP.
      vNombreEmpresaCREDITO VARCHAR2(40) :='COOPERATIVA DE AHORRO Y CREDITO PACIFICO';
      vDetalleCREDITO    VARCHAR2(400);
      vTipoRegDetCREDITO VARCHAR2(2):= 'DD';
      vContaCREDITO      NUMBER(6):=0;
      vContaCREDITO1      NUMBER(6):=0;  -- Contador usado para ser campo unico en Nro. Documento Pago
      vSumaCREDITO       NUMBER(15,2):=0;
      vRellenoCabcredito VARCHAR2(157):= ' ';
      --
      v01  VARCHAR2(2);
      v02  VARCHAR2(2);
      v03  VARCHAR2(2);
      v04  VARCHAR2(2);
      v05  VARCHAR2(2);
      v06  VARCHAR2(2);
      v07  VARCHAR2(2):='00';
      v08  VARCHAR2(2):='00';
      vCtaemp varchar2(9);
      vMinimo NUMBER(15,2) :=0;
      vNumerocuota VARCHAR2 (2);
      vfechabloqueo date;
      nCodigoRecaudador Number(7);
      vCodigoProyecto Varchar2(10);

      -- Datos Scotiabank
      vCabeceraScotiabank varchar2(360);
      vTipoRegistroCabScotiabank VARCHAR2(1)  := 'H';
      vTipoRegistroDetScotiabank VARCHAR2(1)  := 'D';
      vCuentaCabScotiabank VARCHAR2(14)  := '00044460000924';
      vRellenoCabScotiabank1 VARCHAR2(3) :='000';
      vDiasMoraScotiabank VARCHAR2(3) :='000';
      vDetalleScotiabank    VARCHAR2(300);
      vMonedaScotiabank VARCHAR2(4);
      vPieScotiabank VARCHAR2(360):= ' ';
      vTipoRegistroPieScotiabank VARCHAR2(1):= 'C';
      vCuentaAbonoPieScotiabank varchar(14):=' ';
      vCodigoConcepto VARCHAR(2):='01';
      vIterador      NUMBER(6):=0;      
      -- Datos Scotiabank
      
      vSumaDetalle NUMBER(15,2):=0;
      
            -- Ini-Datos Banco de la Nacion--
      vDetalleBn      VARCHAR2(400):= ' ';
      
      -- Fin-Datos Banco de la Nacion--
      
    --Datos IBK--

    --Tipos de Registo
    vCabeceraIBK        VARCHAR2(400);
    vCuotaIBK           VARCHAR2(400);
    vDetalleIBK         VARCHAR2(400);
    --

    vTipoCabeceraIBK       VARCHAR2(2) := '11';
    vCodGrupoIBK           VARCHAR2(2) := '21';
    vCodRubroIBK           VARCHAR2(2) := '03';
    vCodEmpresaIBK         VARCHAR2(3) := '953';
    vCodServicioIBK        VARCHAR2(2) := '01';
    vCodUnicoIBK           VARCHAR2(10) := '0005457247';

    vTipoCuotaIBK          VARCHAR2(2) := '12';

    vTipoDetalleIBK        VARCHAR2(2) := '13';

    vMonedaIBK          VARCHAR2(2) := '12';

    vContaCreditoIBK      NUMBER(6):=0; --Cuenta solo < 10M
	vSumaCreditoIBK     NUMBER(15,2) := 0; --Suma solo < 10M

    --Fin Datos IBK--

    --Datos GLOBOKAS--
	vCodSer  VARCHAR2(2) := '33';
	vCodEmp  VARCHAR2(2) := '33';
	vSumaTotalGlobokas NUMBER(15, 2):=0;

	vMonedaGlobokas       VARCHAR2(2);
	vDetalleGlobokas      VARCHAR2(400):= ' ';
	vCabeceraGlobokas      VARCHAR2(400):= ' ';
    --Fin Datos GLOBOKAS--

    BEGIN
         DELETE FROM recaudafinanciero;
         --COMMIT;
         DELETE FROM recaudacontinental;
         --COMMIT;
         DELETE FROM recaudacredito;
         --COMMIT;
         DELETE FROM recaudascotiabank;
         --COMMIT;
         DELETE FROM recaudabn;
         --COMMIT;
         IF PIMONEDA = 1 THEN
          DELETE FROM recaudainterbanksoles;
         ELSE
          DELETE FROM recaudainterbankdolares;
         END IF;
         DELETE FROM recaudaglobokas;
         --COMMIT;
         DELETE RECAUDAPTP
         WHERE FechaEnvio=PIFECHA;
         --
         COMMIT;
         --
         EXECUTE IMMEDIATE 'ALTER SESSION set NLS_LANGUAGE = "SPANISH" ';
         EXECUTE IMMEDIATE 'ALTER SESSION set NLS_TERRITORY = "SPAIN" ';
         --
         vTotalAmortizacion := 0;
         vMontoadeudado     := 0;
         vContaCREDITO      := 0;
         vContaCREDITO1      := 0;
         vSumaCREDITO       := 0;
         vSumaDetalle        := 0;
         --
         FOR x IN detalle LOOP
             vTotalAmortizacion  := NVL(x.SALDOCAPITAL,0) + NVL(x.SEGUROINTERES,0) +  NVL(x.APORTES,0) + NVL(x.REAJUSTE,0);
             vMontoadeudado :=  vTotalAmortizacion + NVL(x.SALDOINTERES,0) +  NVL(x.SALDOMORA,0);
             IF NVL (vTotalAmortizacion, 0) > 0 THEN
                IF NVL (x.Monto_Minimo, 0) >= 0
                   AND NVL (x.SALDOCAPITAL, 0) >= 0
                   AND NVL (x.SALDOINTERES, 0) >= 0
                   AND NVL (x.SALDOMORA, 0) >= 0
                   AND NVL (x.SEGUROINTERES, 0) >= 0
                   AND NVL (x.APORTES, 0) >= 0
                   AND NVL (x.REAJUSTE, 0) >= 0
                THEN
                    IF x.Tipoproducto <> 'PTP' THEN
                       --vTotalAdeudado := vTotalAdeudado + vMontoadeudado;
                       --vTotalMinimo   := vTotalMinimo + vminimo;
                       vContaCREDITO := vContaCREDITO + 1;
                       vSumaCREDITO  := vSumaCREDITO + vMontoadeudado;
                       
             		      vSumaTotalGlobokas := vSumaTotalGlobokas + CEIL(vMontoadeudado); --GLOBOKAS solo acepta importes enteros (sin decimal)

                       IF NVL(vMontoadeudado, 0) < 10000000 THEN -- IBK solo acepta importes de 9 digitos (7 enteros y 2 decimales)
                        vContaCreditoIBK := vContaCreditoIBK + 1;
                        vSumaCreditoIBK  := vSumaCreditoIBK + FLOOR(NVL(vMontoadeudado, 0)*100); --Sin separador Decimal
                       END IF;

                        --Insercion de la data enviada en los txt de recaudacion en la tabla RECAUDACIONENVIO -- David Chara 16-01-2020 
                        DECLARE
                        ITEMENVIO NUMBER;
                        PERIODOSOLICITUDENVIO VARCHAR2(40);
                        TIPOCUOTAENVIO VARCHAR2(3);
                        MONEDAENVIO VARCHAR2(10);
                        BEGIN
                        SELECT MAX(ITEM) INTO ITEMENVIO FROM RECAUDACIONENVIO WHERE FECHA=TRUNC(TO_DATE(SYSDATE,'DD/MM/YYYY'));
                        
                        PERIODOSOLICITUDENVIO := SUBSTR(x.PAGO_ID, -14, 4);
                        TIPOCUOTAENVIO := SUBSTR(x.PAGO_ID, -3, 3);
                        
                        IF PIMONEDA = 1 THEN
                            MONEDAENVIO :='PEN';
                        ELSE
                             MONEDAENVIO :='USD';             
                        END IF;
                        
                        
                        INSERT INTO RECAUDACIONENVIO (FECHA,ITEM,PERIODOSOLICITUD,NUMEROSOLICITUD,REFERENCIA,NROCUOTA,TIPOCUOTA,CAPITAL,MORA,INTERES,TOTAL,FECHAGENERACION,MONEDA,FECHAVENCIMIENTO)
                        VALUES ( TRUNC(TO_DATE(SYSDATE,'DD/MM/YYYY')), NVL(ITEMENVIO,0) +1,PERIODOSOLICITUDENVIO, x.NumeroSolicitud, x.NOM_CLIENTE, x.numerocuota, 
                        TIPOCUOTAENVIO,x.SALDOCAPITAL,x.SALDOMORA,x.SALDOINTERES,vMontoadeudado,SYSDATE,MONEDAENVIO,TO_CHAR(x.fechavencimiento,'DD/MM/YYYY'));
                        END;
                       --Insercion de la data enviada en los txt de recaudacion en la tabla RECAUDACIONENVIO -- David Chara 16-01-2020

                    END IF;
                END IF;
             END IF;

         END LOOP;
    dbms_output.put_line(' vContaCREDITO D'||vContaCREDITO);
    dbms_output.put_line(' vSumaCREDITO D'||vSumaCREDITO);
         --
         IF PIMONEDA = 1 THEN
            vmoneda :='PEN';
            vNumeroClase := 500;
            vCtaemp:= '296811343';
            vMonedaCREDITO := '0';
            vMonedaScotiabank := '0000';
                vCuentaAbonoPieScotiabank := '00044460000924';
            --vCuentaCabScotiabank := '00044460000924';
            vCodigoAfiliacion := '2616734';
            vMonedaIBK := '01';
		        vMonedaGlobokas := '1';             --GLOBOKAS Solo Soles
          INSERT INTO recaudainterbanksoles ( tipo, campo ) VALUES ( 0, vSumaCreditoIBK ); 
          INSERT INTO recaudainterbanksoles ( tipo, campo ) VALUES ( 1, vContaCreditoIBK ); 
         ELSE
             vmoneda :='USD';
             vNumeroClase := 600;
             vCtaemp:= '296811350';
             vMonedaCREDITO := '1';
             vMonedaScotiabank := '0001';
             vCuentaAbonoPieScotiabank := '01044460000621';
             --vCuentaCabScotiabank := '01044460000621';
             vCodigoAfiliacion := '2617465';
            vMonedaIBK := '10';
		        --vMonedaGlobokas := '2';             --GLOBOKAS Futuro +Dolares
          INSERT INTO recaudainterbankdolares ( tipo, campo ) VALUES ( 0, vSumaCreditoIBK ); 
          INSERT INTO recaudainterbankdolares ( tipo, campo ) VALUES ( 1, vContaCreditoIBK ); 
         END IF;
         --
         vCabecera := vTipoRegistroCab||
                      vRucEmpresa||
                      vNumeroClase||
                      vMoneda||
                      vFechaFactura||
                      RPAD(TRIM(vVersion),3,'0')||
                      RPAD(vRellenoCab,330,' ');
         --
         vCabeceraCredito := vTipoRegistroCabCREDITO||
                             vSucursalCREDITO||
                             vMonedaCREDITO||
                             vCodigoAfiliacion||
                             vTipoValidaCREDITO||
                             vNombreEmpresaCREDITO||
                             TO_CHAR(HOY,'RRRRMMDD')||
                             LPAD(vContaCREDITO,9,'0')||
                             LPAD(NVL(TRIM(SUBSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),1,INSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),'.')-1))||
                             TRIM(SUBSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),INSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),'.')+1,2)),0),15,'0')||
                             'R'||
                             '000000'||
                             RPAD(vRellenoCab,157,' ');


         vCabeceraScotiabank := vTipoRegistroCabScotiabank||
                             vCuentaCabScotiabank||      --* Numero de cuenta en soles o dolares
                             (CASE WHEN (PIMONEDA) = 1 THEN '001' ELSE '002' END) ||     --* Libre desde el 001-999
                             LPAD(vContaCREDITO,7,'0') ||  -- Cantidad de registros del detalle
                             LPAD((CASE WHEN (PIMONEDA) = 1 THEN (NVL(TRIM(SUBSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),1,INSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),'.')-1))||
                                   TRIM(SUBSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),INSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),'.')+1,2)),0))  ELSE '0' END),17,'0') ||  -- Suma total soles   --*
                             --LPAD((CASE WHEN (:PIMONEDA) = 2 THEN vSumaCREDITO ELSE '0' END) ,17,'0') ||  -- Suma total dolares --*
                             LPAD((CASE WHEN (PIMONEDA) = 2 THEN (NVL(TRIM(SUBSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),1,INSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),'.')-1))||
                                   TRIM(SUBSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),INSTR(TO_CHAR(vSumaCREDITO,'9999999999999.99'),'.')+1,2)),0)) ELSE '0' END) ,17,'0') ||  -- Suma total dolares --*
                             vRucEmpresa    ||  -- Ruc Empresas
                             TO_CHAR(HOY,'YYYYMMDD') || -- Fecha envio informaci?n
                             TO_CHAR(HOY,'YYYYMMDD') || -- Fecha de vigencia de la informaci?n
                              vRellenoCabScotiabank1 ||  -- Relleno
                              vDiasMoraScotiabank  ||   -- Dias mora por defecto 900 (sin mora)
                              '00'  ||                  -- Tipo mora 00=No aplica
                              LPAD(0,11,'0') ||  -- Mora flat tiene que ver con el tipo mora
                              LPAD(0,8,'0') ||  -- Porcentaje mora tiene que con el tipo mora
                              LPAD(0,11,'0') ||  -- Monto fijo tiene que con el tipo mora
                              '00' ||  -- Tipo descuento
                              LPAD(0,11,'0') ||  -- Monto a descontar
                              LPAD(0,8,'0') ||  -- Porcentaje descuento
                              LPAD(0,3,'0') ||  -- Dias descuento
                             TO_CHAR(HOY,'YYYYMMDD') || -- Fecha inicio de cargo automatico         --* Sino se define aca debe ir en el detalle
                             TO_CHAR(HOY,'YYYYMMDD') || -- Fecha fin de cargo automatico            --* Sino se define aca debe ir en el detalle
                             '1' || -- Mismo  inicio y fin de cargo                                     --* Sino se define aca debe ir en el detalle
                             RPAD(' ',124,' ') ||
                             '*';

            vCabeceraGlobokas := '99999999'||
                  TRIM(TO_CHAR(vConteoTotal,'0000000'))||
                  TRIM(TO_CHAR((CASE WHEN (PIMONEDA) = 1 THEN vSumaTotalGlobokas * 100 ELSE 0 END), '000000000000000000'))||
                  TRIM(TO_CHAR((CASE WHEN (PIMONEDA) = 2 THEN vSumaTotalGlobokas * 100 ELSE 0 END), '000000000000'))||
                  TRIM(TO_CHAR(HOY+1, 'YYYYMMDD'))||
                  vCodSer|| --Codigo de la empresa SVC
                  '00000000'|| --Fecha Vencimiento
                  '00000000000000000000000000000000000000000';  --GLOBOKAS
         --
         INSERT INTO recaudacontinental ( campo ) VALUES ( vCabecera );
         INSERT INTO recaudacredito ( campo ) VALUES ( vCabeceraCredito );
         INSERT INTO recaudascotiabank ( campo ) VALUES ( vCabeceraScotiabank );
         INSERT INTO recaudaglobokas (campo) VALUES ( vCabeceraGlobokas );  --GLOBOKAS
         --
         vTotalAmortizacion := 0;
         vMontoadeudado     := 0;
         --
         FOR x IN detalle LOOP
             vContaCREDITO1 := vContaCREDITO1 + 1;
             vIterador := vIterador + 1;
             vTipoPersona := pkg_persona.f_obt_tipopersona( x.codigopersona );
             IF vTipoPersona = 1 THEN
                vNumerodocumento := pkg_personanatural.F_OBT_NUMERODOCUMENTOID( x.codigopersona );
                vTipodocumento   := 'L';
                vTipodocumentofin := 'C';
             ELSE
                 vNumerodocumento := pkg_persona.F_OBT_NUMERORUC( x.codigopersona );
                 vTipodocumento   := 'R';
                 vTipodocumentofin := 'R';
             END IF;
             --
             vTotalAmortizacion  := NVL(x.SALDOCAPITAL,0) + NVL(x.SEGUROINTERES,0) +  NVL(x.APORTES,0) + NVL(x.REAJUSTE,0);
             vMontoadeudado :=  vTotalAmortizacion + NVL(x.SALDOINTERES,0) +  NVL(x.SALDOMORA,0);
             vMinimo  := vMontoadeudado;
              
             --
             IF vTotalAmortizacion > 0 THEN
                v01 := '01';
             ELSE
                 v01 := '00';
             END IF;
             --
             IF x.SALDOINTERES > 0 THEN
                v02 := '02';
             ELSE
                v02 := '00';
             END IF;
             --
             IF x.SALDOMORA > 0 THEN
                v03 := '03';
             ELSE
                v03 := '00';
             END IF;
             --
             v04 := '00';
             v05 := '00';
             v06 := '00';
             --
             IF x.fechavencimiento> TRUNC(HOY) THEN
                vfechabloqueo := x.fechavencimiento;
             ELSE
                vfechabloqueo := TRUNC(HOY);
             END IF;
             --
             IF LENGTH(TRIM(x.numerocuota)) >= 3  THEN
                vNumerocuota := SUBSTR (TRIM(x.numerocuota),2,2);
             ELSE
                vNumerocuota := x.numerocuota;
             END IF;
             --Archivo PTP
             vDetalle := vTipoRegistroDet||                                    ------- LINEA 1
                         --RPAD(TRIM(NVL(x.nom_cliente,'')),30,' ')||      ------- LINEA 2
                         RPAD(TRIM(NVL(SISGODBA.F_GEN_CADENALIMPIA(x.nom_cliente),'')),30,' ')||      ------- LINEA 2                     
                         --RPAD(TRIM(x.pago_id),48,' ')||                       ------- LINEA 3
                         RPAD(TRIM(x.PAGO_ID_CONTINENTAL),48,' ')||                       ------- LINEA 3                         
                         TO_CHAR(x.fechavencimiento,'RRRRMMDD')||  ------- LINEA 4
                         TO_CHAR(vfechabloqueo,'RRRRMMDD')||             ------- LINEA 5
                         vNumerocuota|| --TO_CHAR(SYSDATE,'MM')|| ------- LINEA 6
                         LPAD(NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0),15,'0')||                                          ------- LINEA 7
                         LPAD(NVL (TRIM(SUBSTR(TO_CHAR(vMinimo,'9999999999999.99'),1,INSTR(TO_CHAR(vMinimo,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(vMinimo,'9999999999999.99'),INSTR(TO_CHAR(vMinimo,'9999999999999.99'),'.')+1,2)),0)
                         ,15,'0')||
                         '00000000000000000000000000000000'||   v01||    ------- LINEA 8
                         LPAD ( NVL (TRIM(SUBSTR(TO_CHAR(vTotalAmortizacion,'9999999999999.99'),1,INSTR(TO_CHAR(vTotalAmortizacion,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(vTotalAmortizacion,'9999999999999.99'),INSTR(TO_CHAR(vTotalAmortizacion,'9999999999999.99'),'.')+1,2)),0)
                         ,14,'0')||
                         v02||                                                   ------- LINEA 9
                         LPAD ( NVL (TRIM(SUBSTR(TO_CHAR(x.SALDOINTERES,'9999999999999.99'),1,INSTR(TO_CHAR(x.SALDOINTERES,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(x.SALDOINTERES,'9999999999999.99'),INSTR(TO_CHAR(x.SALDOINTERES,'9999999999999.99'),'.')+1,2)),0)
                         ,14,'0')||
                         v03||                                             ------- LINEA 10
                         LPAD ( NVL (TRIM(SUBSTR(TO_CHAR(x.SALDOMORA,'9999999999999.99'),1,INSTR(TO_CHAR(x.SALDOMORA,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(x.SALDOMORA,'9999999999999.99'),INSTR(TO_CHAR(x.SALDOMORA,'9999999999999.99'),'.')+1,2)),0),14,'0')||
                         v04||                                                 ------- LINEA 11
                         LPAD ( NVL (TRIM(SUBSTR(TO_CHAR(0,'9999999999999.99'),1,INSTR(TO_CHAR(0,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(0,'9999999999999.99'),INSTR(TO_CHAR(0,'9999999999999.99'),'.')+1,2)),0),14,'0')||
                         v05||                                                ------- LINEA 12
                         LPAD (NVL(TRIM(SUBSTR(TO_CHAR(0,'9999999999999.99'),1,INSTR(TO_CHAR(0,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(0,'9999999999999.99'),INSTR(TO_CHAR(0,'9999999999999.99'),'.')+1,2)),0) ,14,'0')||
                         v06||                                                 ------- LINEA 13
                         LPAD( NVL (TRIM(SUBSTR(TO_CHAR(0,'9999999999999.99'),1,INSTR(TO_CHAR(0,'9999999999999.99'),'.')-1))||
                         TRIM(SUBSTR(TO_CHAR(0,'9999999999999.99'),INSTR(TO_CHAR(0,'9999999999999.99'),'.')+1,2)),0),14,'0')||
                         v07||                                                 ------- LINEA 14
                         '00000000000000'||
                         v08||                                                 ------- LINEA 15
                         '00000000000000'||                            ------- LINEA 16
                         --RPAD(TRIM(vRellenoDet1),20,'0')||         ------- LINEA 17
                         RPAD(TRIM('0'),20,'0')||         ------- LINEA 17
                         vTipodocumento||                                  ------- LINEA 18
                         LPAD(TRIM(vNumeroDocumento),15,'0')||------- LINEA 19
                         --RPAD(vRellenoDet2,36,' ');
                         RPAD(' ',36,' ');
                         -- Inicio Financiero WDD
             vDetallefin := 'CO'|| CHR(9) ||
                            vCtaemp || CHR(9) ||
                            vNumeroDocumento || CHR(9) ||
                            vmoneda  || CHR(9) ||
                            NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                            TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0)
                            || CHR(9) ||
                            'REC' || CHR(9) ||
                            '35' || CHR(9) ||
                            vTipodocumentofin  || CHR(9) ||
                            vNumeroDocumento || CHR(9) ||
                            '"' || RPAD(TRIM(NVL( SISGODBA.F_GEN_CADENALIMPIA(x.nom_cliente),'')),30,' ')||'"' || CHR(9) ||
                            RPAD(TRIM(x.pago_id),40,' ')  || CHR(9) ||
                            TRIM(x.pago_id);
             --
                vDetalleCREDITO := vTipoRegDetCREDITO||
                                vSucursalCREDITO||
                                vMonedaCREDITO||
                                vCodigoAfiliacion||
                                LPAD(TRIM(x.codigosocio),14,'0')||
                                (RPAD(SISGODBA.F_GEN_CADENALIMPIA(x.nom_cliente) ,40,' ')) ||
                                LPAD(x.pago_id_2 || LPAD(TRIM(x.numerocuota),3,'0') ,30,'0')||
                                TO_CHAR(HOY,'YYYYMMDD')||
                                TO_CHAR(x.fechavencimiento,'YYYYMMDD')||
                                LPAD( NVL(TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                                TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0),15,'0')||
                                /*LPAD(NVL(TRIM(SUBSTR(TO_CHAR(x.saldomora,'9999999999999.99'),1,INSTR(TO_CHAR(x.saldomora,'9999999999999.99'),'.')-1))||
                                TRIM(SUBSTR(TO_CHAR(x.saldomora,'9999999999999.99'),INSTR(TO_CHAR(x.saldomora,'9999999999999.99'),'.')+1,2)),0),15,'0')||
                                LPAD(NVL (TRIM(SUBSTR(TO_CHAR(x.monto_minimo,'9999999999999.99'),1,INSTR(TO_CHAR(x.monto_minimo,'9999999999999.99'),'.')-1))||
                                TRIM(SUBSTR(TO_CHAR(x.monto_minimo,'9999999999999.99'),INSTR(TO_CHAR(x.monto_minimo,'9999999999999.99'),'.')+1,2)),0),9,'0')||*/
                                LPAD('0',15,'0')||
                                LPAD(NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                                TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0),9,'0')||
                                'A'||
                                LPAD(vContaCREDITO1,20,'0')||
                                LPAD(TRIM(vNumeroDocumento),16,'0')||
                                RPAD(vRellenoCabCREDITO,61,' ')
                                ;
                --
               vDetalleScotiabank := vTipoRegistroDetScotiabank ||
                                   vCuentaCabScotiabank ||   -- Cuenta empresa           --**    Es igual que la cabecera
                                   (CASE WHEN (PIMONEDA) = 1 THEN '001' ELSE '002' END)  || -- Codigo del servicio      --**    Es igual que la cabecera
                                   RPAD(TRIM(x.codigosocio),15,' ') ||    -- Codigo del usuario    --**    Se esta enviado el codigo socio
                                   LPAD(x.PeriodoSolicitud || x.NumeroSolicitud || x.numerocuota ,15,'0') ||        -- Numero del recibo     --**    Se le envia un iterante
                                   LPAD(' ',11,' ')  ||-- Ruc de agrupacion deuda instituci?n  --** Se le esta enviado el dni o ruc
                                   '0' ||
                                   vMonedaScotiabank  ||            -- Moneda
                                   --(RPAD(SISGODBA.F_GEN_CADENALIMPIA(TRIM(NVL(replace(replace(replace(replace(replace(replace(replace(replace(x.nom_cliente,'A','A'),'E','E'),'I','I'),'O','O'),'U','U'),'?','N'),'?',''),'U','U'),''))) ,20,' ')) ||   -- Nombre usuario
                                   (RPAD(SISGODBA.F_GEN_CADENALIMPIA(x.nom_cliente) ,20,' ')) ||   -- Nombre usuario                               
                                   RPAD(x.pago_id,30,' ') ||          -- Referencia recibo
                                   LPAD(vCodigoConcepto,2,'0') || -- Codigo concepto 1
                                   LPAD(NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                                   TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0),11,'0')||    --Importe concepto 1  --**
                                   LPAD('0',2,'0')  ||-- Codigo concepto 2
                                   LPAD('0',11,'0') ||-- Importe concepto 2
                                   LPAD('0',2,'0')  || -- Codigo concepto 3
                                   LPAD('0',11,'0') || -- Importe concepto 3
                                   LPAD('0',2,'0')  ||-- Codigo concepto 4
                                   LPAD('0',11,'0') || -- Importe concepto 4
                                   LPAD('0',2,'0')  || -- Codigo concepto 5
                                   LPAD('0',11,'0')  || -- Importe concepto 5
                                   LPAD('0',2,'0')  || -- Codigo concepto 6
                                   LPAD('0',11,'0') ||-- Importe concepto 6
                                   LPAD(NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                                   TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0),15,'0')||    -- Sumatoria total importe
                                   LPAD(NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                                   TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0),15,'0')||      -- Saldo del documento
                                   LPAD( '0',8,0) || -- Salo para pagos parciales  --*
                                   --'0' || --Orden Cronologico                      --*
                                   (CASE WHEN SUBSTR(x.pago_id,-3)='ACT' THEN '1' ELSE '0' END) ||
                                   TO_CHAR(HOY,'YYYYMMDD')||  -- Fecha emisi?n  --*
                                   TO_CHAR(x.fechavencimiento,'YYYYMMDD') ||    -- Fecha vencimiento    
                                   LPAD('900',3,'0') ||  -- Dias de prorroga                            
                                   LPAD(' ',14,' ') ||  -- Cuenta cargo                               
                                   '01' || -- Cargo automatico 
                                   'R' ||  -- Tipo afiliacion
                                   LPAD(' ',8,' ') || --TO_CHAR(SYSDATE,'YYYYMMDD') || -- Fecha inicio Cobro automatico      --*
                                   LPAD(' ',8,' ') || --TO_CHAR(SYSDATE,'YYYYMMDD') || -- Fecha fin Cobro automatico         --*
                                   LPAD(' ',6,' ') || -- Relleno
                                   '*'
                                   ;
                                   
             vDetalleIBK :=  vTipoDetalleIBK ||                             --Tipo de registro
                        RPAD(TRIM(x.codigosocio), 20, ' ') ||       --Código de deudor
                        RPAD(TRIM(SUBSTR(PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(x.codigopersona), 1, 30)), 30, ' ') ||
                                                                    --Nombre del deudor
                        RPAD(x.PeriodoSolicitud, 10, ' ') ||        --Referencia 1
                        RPAD(CASE SUBSTR(x.pago_id, -3, 3) WHEN 'ATR' THEN 'ATRASADO' WHEN 'ACT' THEN 'VIGENTE' ELSE ' ' END, 10, ' ') ||
                                                                    --Referencia 2
                        'A' ||                                      --Tipo de Operación
                        RPAD(x.NumeroSolicitud || CASE SUBSTR(x.pago_id, -3, 3) WHEN 'ACT' THEN 'V' ELSE 'A' END, 8, ' ') ||          --Código de cuota
                        TO_CHAR(HOY, 'YYYYMMDD') ||                 --Fecha de emisión
                        TO_CHAR(x.fechavencimiento, 'YYYYMMDD') ||  --Fecha de vencimiento
                        RPAD(x.numerocuota || '-' || SUBSTR(x.pago_id, -3, 3) || '-' || x.PeriodoSolicitud, 15, ' ') ||
                                                                    --Número de documento
                        vMonedaIBK ||                               --Moneda de la deuda
                        LPAD(FLOOR((NVL(vMontoadeudado, 0) * 100)), 9, '0') ||
                                                                    --Importe del concepto 1
                        LPAD('0',  9, '0') ||                       --Importe del concepto 2
                        LPAD('0',  9, '0') ||                       --Importe del concepto 3
                        LPAD('0',  9, '0') ||                       --Importe del concepto 4
                        LPAD('0',  9, '0') ||                       --Importe del concepto 5
                        LPAD('0',  9, '0') ||                       --Importe del concepto 6
                        LPAD('0',  9, '0') ||                       --Importe del concepto 7
                        LPAD(' ',  1, ' ') ||                       --Tipo de la cuenta Principal
                        LPAD(' ',  3, ' ') ||                       --Producto de la cuenta Principal
                        LPAD(' ',  2, ' ') ||                       --Moneda de la cuenta Principal
                        LPAD(' ', 20, ' ') ||                       --Numero de la cuenta Principal
                        LPAD(' ', 15, '0') ||                       --Importe a abonar cuenta 1
                        LPAD(' ',  1, ' ') ||                       --Tipo de la cuenta Secundaria
                        LPAD(' ',  3, ' ') ||                       --Producto de la cuenta Secundaria
                        LPAD(' ',  2, ' ') ||                       --Moneda de la cuenta Secundaria
                        LPAD(' ', 20, ' ') ||                       --Numero de la cuenta Secundaria
                        LPAD(' ', 15, '0') ||                       --Importe a abonar cuenta 2
                        LPAD(' ', 67, ' ') ||                       --Glosa Particular
                        LPAD(' ', 68, ' ') ||                       --Libre
                        '02' ||                                     --Tipo Formato
                        '0000';                                     --Código fijo
                                
                  vDetalleGlobokas := vCodSer|| --SER Codigo del Servicio SVC
                                      '00'|| --SECCION fijo
                                      TRIM(LPAD(x.PeriodoSolicitud ||x.NumeroSolicitud || '-' || SUBSTR(x.pago_id, -3, 3), 15, '0'))|| --RECIBO Numero Recibo
                                      TRIM(TO_CHAR(x.fechavencimiento,'YYYYMMDD'))|| --FechaVigencia
                                      vMonedaGlobokas|| --TipoMoneda
                                      --TRIM(LPAD(x.CODIGOSOCIO, 15, '0'))|| --NUMSER campo de busqueda
                                      CASE PKG_PERSONA.F_OBT_TIPOPERSONA(x.codigopersona)
                                          WHEN 1 THEN
                                             LPAD(PKG_PERSONANATURAL.F_OBT_NUMERODOCUMENTOID(x.codigopersona), 15, '0') --NUMSER campo de busqueda --25.02.2021
                                          WHEN 2 THEN
                                             LPAD(PKG_PERSONA.F_OBT_NUMERORUC(x.codigopersona), 15, '0') --NUMSER campo de busqueda --25.02.2021
                                       END ||
                                       RPAD(SUBSTR(PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(x.codigopersona), 1, 30), 30, ' ') || --NUMABO Nombre Cliente
                                      '000'|| --NROCEN fijo
                                      TRIM(LPAD(vNumerocuota||TO_CHAR(x.fechavencimiento,'YYMMDD')||TO_CHAR(HOY,'YYMMDD'), 15, '0'))|| --NUMCLI
                                      TRIM(TO_CHAR(CEIL(vMontoadeudado)*100, '0000000000'))|| --TOTAL Importe Facturado a Cobrar
                                      vCodEmp|| --CODEMP Codigo de la empresa SVC
                                      'R'; --GLOBOKAS
                                
                  vDetalleBn := 'BN'|| CHR(9) ||
                            vCtaemp || CHR(9) ||
                            vNumeroDocumento || CHR(9) ||
                            vmoneda  || CHR(9) ||
                            NVL (TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),1,INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')-1))||
                            TRIM(SUBSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),INSTR(TO_CHAR(vMontoadeudado,'9999999999999.99'),'.')+1,2)),0)
                            || CHR(9) ||
                            'REC' || CHR(9) ||
                            '35' || CHR(9) ||
                            vTipodocumentofin  || CHR(9) ||
                            vNumeroDocumento || CHR(9) ||
                            '"' || RPAD(TRIM(NVL( SISGODBA.F_GEN_CADENALIMPIA(x.nom_cliente),'')),30,' ')||'"' || CHR(9) ||
                            RPAD(TRIM(x.pago_id),40,' ')  || CHR(9) ||
                            TRIM(x.pago_id);
             --
             IF NVL (vTotalAmortizacion, 0) > 0 THEN
                IF NVL (x.Monto_Minimo, 0) >= 0
                   AND NVL (x.SALDOCAPITAL, 0) >= 0
                   AND NVL (x.SALDOINTERES, 0) >= 0
                   AND NVL (x.SALDOMORA, 0) >= 0
                   AND NVL (x.SEGUROINTERES, 0) >= 0
                   AND NVL (x.APORTES, 0) >= 0
                   AND NVL (x.REAJUSTE, 0) >= 0
                THEN
                    IF x.Tipoproducto <> 'PTP' THEN
                       vTotalAdeudado := vTotalAdeudado + vMontoadeudado;
                       vTotalMinimo   := vTotalMinimo + vminimo;
                       vConteoTotal   :=  vConteoTotal + 1;
                       vSumaDetalle := vSumaDetalle + vMontoadeudado;
                       --
                       INSERT INTO recaudafinanciero (campo) VALUES (vDetallefin);
                       INSERT INTO recaudacontinental ( campo ) VALUES (vDetalle);
                       INSERT INTO recaudaCREDITO ( campo ) VALUES ( vDetalleCREDITO );
                       INSERT INTO recaudaScotiabank ( campo ) VALUES ( vDetalleScotiabank );
                        IF NVL(vMontoadeudado, 0) < 10000000 THEN
                        
                          IF PIMONEDA = 1 THEN
                            INSERT INTO recaudainterbankSoles ( tipo, campo ) VALUES ( 2, vDetalleIBK );
                          ELSE
                            INSERT INTO recaudainterbankDolares ( tipo, campo ) VALUES ( 2, vDetalleIBK );
                          END IF;
                        END IF;
                       INSERT INTO recaudaglobokas ( campo ) VALUES ( vDetalleGlobokas );  --GLOBOKAS
                    ELSE
                         BEGIN
                            SELECT CodigoRecaudador,CodigoProyecto
                              INTO nCodigoRecaudador,vCodigoProyecto
                              FROM ProyectoCreditosPtp
                             WHERE PeriodoSolicitud=x.PeriodoSolicitud
                               AND NumeroSolicitud=x.NumeroSolicitud;
                         EXCEPTION WHEN OTHERS THEN
                                    nCodigoRecaudador := NULL;
                                    vCodigoProyecto   := NULL;
                         End;
                         INSERT INTO recaudaPTP( fechaenvio,
                                                 codigopersona,
                                                 nombresocio,
                                                 numerodocumentoid,
                                                 periodosolicitud,
                                                 numerosolicitud,
                                                 codigocredito,
                                                 fechavencimiento,
                                                 amortizacion,
                                                 interes,
                                                 mora,
                                                 Reajuste,
                                                 SeguroBien,
                                                 aportes,
                                                 total,
                                                 numerocuota,
                                                 indcuota,
                                                 CodigoRecaudador,
                                                 CodigoProyecto,
                                                 moneda )
                                        VALUES ( TRUNC(HOY),
                                                 x.codigopersona,
                                                 PKG_PERSONA.F_OBT_NOMBRECOMPLETO(x.Codigopersona),
                                                 vNumeroDocumento,
                                                 x.PeriodoSolicitud,
                                                 x.NumeroSolicitud,
                                                 SUBSTR(x.Pago_Id,1,LENGTH(x.Pago_Id)-3),
                                                 x.fechavencimiento,
                                                 x.SaldoCapital,
                                                 x.SaldoInteres,
                                                 x.SaldoMora,
                                                 x.SeguroInteres,
                                                 x.reajuste,
                                                 x.Aportes,
                                                 vMinimo,
                                                 x.numerocuota,
                                                 SUBSTR(x.Pago_Id,26,3),
                                                 nCodigoRecaudador,
                                                 vCodigoProyecto,
                                                 PIMoneda);

                    END IF;
                END IF;
             END IF;
             
             
             
             
         END LOOP;
         --
      --DBMS_OUTPUT.PUT_LINE('vConteoTotal  '||vConteoTotal);
      DBMS_OUTPUT.PUT_LINE('Suma detalle: '|| vSumaDetalle);      
      --DBMS_OUTPUT.PUT_LINE('vTotalMinimo XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX: '|| vTotalMinimo);
      --DBMS_OUTPUT.PUT_LINE('Suma detalle: '|| vSumaDetalle);
      
         --
         vPie := vTipoRegistroPie||
                 LPAD(TRIM(TO_CHAR(vConteoTotal,'999999999')),9,'0')||
                 LPAD(TRIM(SUBSTR(TO_CHAR(vTotalAdeudado,'999999999999999.99'),1,INSTR(TO_CHAR(vTotalAdeudado,'999999999999999.99'),'.')-1))||
                 TRIM(SUBSTR(TO_CHAR(vTotalAdeudado,'999999999999999.99'),INSTR(TO_CHAR(vTotalAdeudado,'999999999999999.99'),'.')+1,2)),18,'0')||
                 LPAD(TRIM(SUBSTR(TO_CHAR(vTotalMinimo,'999999999999999.99'),1,INSTR(TO_CHAR(vTotalMinimo,'999999999999999.99'),'.')-1))||
                 TRIM(SUBSTR(TO_CHAR(vTotalMinimo,'999999999999999.99'),INSTR(TO_CHAR(vTotalMinimo,'999999999999999.99'),'.')+1,2)),18,'0')||
                 RPAD(TRIM(vRellenoPie1),18,'0')||
                 RPAD(vRellenoPie2,295,'  ');

          vPieScotiabank := vTipoRegistroPieScotiabank ||
                  vCuentaCabScotiabank ||     -- Cuenta empresa
                 (CASE WHEN (PIMONEDA) = 1 THEN '001' ELSE '002' END) ||    -- Codigo servicio
                 --'01' ||  -- Codigo concepto este codigo es personalizable      --*
                 LPAD(vCodigoConcepto,2,'0') || -- Codigo concepto 1
                 RPAD('Recaudacion Scotiabank',30,' ') || -- Descripcion concepto
                 '1' || -- Acepta pago parcial              --*
                 vCuentaAbonoPieScotiabank || -- Cuenta de abono     --*
                 RPAD(' ',224,' ')||
                 '*';
         --
         INSERT INTO recaudacontinental ( campo ) VALUES ( vPie );         
         INSERT INTO recaudascotiabank ( campo ) VALUES ( vPieScotiabank );
    EXCEPTION WHEN OTHERS THEN
              POERROR:=1;
              dbms_output.put_line(' error '||sqlerrm);
    END P_GEN_DATOSRECAUDACION;
    --
    FUNCTION F_OBT_INTERESATRASADO( PIPeriodoSolicitud IN NUMBER,
                                    PINumeroSolicitud  IN NUMBER,
                                    PIFecha IN DATE
                                  ) RETURN NUMBER IS
    CURSOR c1 IS
    SELECT periodosolicitud,numerosolicitud,amortizacion,interes,fechavencimiento
      FROM prestamocuotas
     WHERE PeriodoSolicitud=PIPeriodoSolicitud
       AND numerosolicitud=PINumeroSolicitud
       AND estado=2
       AND fechavencimiento<PIFecha
     ORDER BY fechavencimiento ASC;

    nDias Number:=0;
    nInteresDia Number:=0;
    nTasaMensual Number;
    nMonto Number:=0;
    nCuota Number:=0;
    nImporteInteresAtrasado Number:=0;

    BEGIN
         FOR rcur IN c1 LOOP
             BEGIN
                  SELECT tasainteres
                    into nTasaMensual
                    from solicitudprestamo
                   where periodosolicitud=rcur.periodosolicitud
                     and numerosolicitud=rcur.numerosolicitud;
             EXCEPTION WHEN OTHERS THEN
                       nTasaMensual:=0;
             END;
             IF NVL(rcur.amortizacion,0)=0 THEN
                ndias := hoy-rcur.fechavencimiento;
                nInteresDia := POWER((1+nTasaMensual/100),(ndias/30))-1;--*rcur.interes;
                nMonto := ROUND(rcur.interes*nInteresDia,2);
                nCuota := rcur.interes+nMonto;
                nImporteInteresAtrasado := nImporteInteresAtrasado+nCuota;
             END IF;
         END LOOP;
         RETURN(nImporteInteresAtrasado);
    END F_OBT_INTERESATRASADO;
    --
    PROCEDURE P_GEN_ARCHIVOTXT( PICodigoBanco datosbanco.codigobanco%TYPE,
                                PIMonedaBanco syst900.tblcodtab%TYPE
                              ) IS

    v_archivo      UTL_FILE.FILE_TYPE;
    vDirectorio    VARCHAR2(500):='RECAUDABANCOS';
    --vNombrearchivo VARCHAR2(500):='BANCO_'||PICodigoBanco ;
    vNombrearchivo VARCHAR2(500):=' ' ;
    vPlantillaContinental VARCHAR2(300):='@VALOR1@_@VALOR2@_@VALOR3@_@VALORFECHA@';
    vPlantillaFinanciero VARCHAR2(300):='@VALOR1@_@VALORFECHA@';
    vPlantillaBcp VARCHAR2(300):='@VALOR1@@VALOR2@@VALOR3@';
    vPlantillaScotiabank VARCHAR2(300):='@VALOR1@@VALOR2@';
    
    vPlantillaGlobokasnet VARCHAR2(300):='@VALOR1@@VALORFECHA@';
    vPlantillaBn VARCHAR2(300):='@VALOR1@@VALOR2@';


    --
    CURSOR c_continental IS
    SELECT *
      FROM recaudacontinental
     ORDER BY 1;

    CURSOR c_financiero IS
    SELECT *
      FROM recaudafinanciero
     ORDER BY 1;

    CURSOR c_credito IS
    SELECT *
      FROM recaudacredito
     ORDER BY 1;

    CURSOR c_scotiabank IS
    SELECT *
      FROM recaudascotiabank
     ORDER BY 1 DESC;

    CURSOR c_globokasnet IS
    SELECT campo
      FROM recaudaglobokas
     ORDER BY 1 desc;
     
    CURSOR c_bn IS
    SELECT *
      FROM recaudabn
     ORDER BY 1 DESC;

    BEGIN
       -- v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');
        --
        IF PICodigoBanco = 6 THEN

        vNombrearchivo := REPLACE(vPlantillaContinental, '@VALOR1@', 'RC');
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALOR2@', (CASE WHEN PIMonedaBanco = 1 THEN '500' ELSE '600' END));
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALOR3@', (CASE WHEN PIMonedaBanco = 1 THEN 'S' ELSE 'D' END));
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALORFECHA@', TO_CHAR(HOY,'YYYYMMDD'));
        v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');
           FOR x IN c_continental LOOP
               --UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(9));
               UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(13));
           END LOOP;
        END IF;
        --
        IF PICodigoBanco = 9 THEN
        vNombrearchivo := REPLACE(vPlantillaFinanciero, '@VALOR1@', (CASE WHEN PIMonedaBanco=1 THEN '27279' ELSE '27533' END));
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALORFECHA@', TO_CHAR(HOY,'YYYYMMDD'));
        v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');
           FOR x IN c_financiero LOOP
               UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(13));
           END LOOP;
        END IF;
        --
        IF PICodigoBanco = 3 THEN
        vNombrearchivo := REPLACE(vPlantillaBcp, '@VALOR1@', 'CREP');
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALOR2@', (CASE WHEN PIMonedaBanco=1 THEN '6221' ELSE  '6220' END));
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALOR3@', (CASE WHEN PIMonedaBanco=1 THEN '3001' ELSE  '3002' END));
        v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');

           FOR x IN c_credito LOOP
                UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(13));
           END LOOP;
        END IF;

        IF PICodigoBanco = 5 THEN
        vNombrearchivo := REPLACE(vPlantillaScotiabank, '@VALOR1@', 'COOPAH');
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALOR2@', (CASE WHEN PIMonedaBanco=1 THEN 'SO' ELSE  'DO' END));
        v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');

           FOR x IN c_scotiabank LOOP
                UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(13));
           END LOOP;
        END IF;

        --Globokasnet / no es un banco
         IF PICodigoBanco = 16969 THEN
        vNombrearchivo := REPLACE(vPlantillaGlobokasnet, '@VALOR1@', 'SCB33');
          vNombrearchivo :=REPLACE(vNombrearchivo, '@VALORFECHA@', TO_CHAR(HOY, 'MMDD'));
        v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');

           FOR x IN c_globokasnet LOOP
                UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(13));
           END LOOP;
        END IF;
        --

         IF PICodigoBanco = 2 THEN
        vNombrearchivo := REPLACE(vPlantillaBn, '@VALOR1@', 'COOPAH');
        vNombrearchivo := REPLACE(vNombrearchivo, '@VALOR2@', (CASE WHEN PIMonedaBanco=1 THEN 'SO' ELSE  'DO' END));
        v_archivo := UTL_FILE.FOPEN(vDirectorio,vNombrearchivo||'.TXT','W');

           FOR x IN c_bn LOOP
                UTL_FILE.PUT_LINE(v_archivo,x.campo||CHR(13));
           END LOOP;
        END IF;
        --

        UTL_FILE.FCLOSE(v_archivo);
    EXCEPTION WHEN OTHERS THEN
                   UTL_FILE.FCLOSE(v_archivo);
        DBMS_OUTPUT.PUT_LINE('ERROR GENERAR TXT IDBANCO:' ||PICodigoBanco||' IDMONEDA:' ||PIMonedaBanco);
    END P_GEN_ARCHIVOTXT;
    --
PROCEDURE P_GEN_GENERARARCHIVOS (PIFECHA IN DATE) IS

CURSOR moneda IS
SELECT tblcodarg
  FROM  SYST900
 WHERE tblcodtab=22;


CURSOR banco IS
SELECT tblcodarg
  FROM  SYST900
 WHERE tblcodtab=39
 AND tblcodarg IN(2,3,6,9,5);

vError NUMBER;

BEGIN

    FOR m IN moneda LOOP 
 
  Pkg_RecaudacionEnvio.P_Gen_DatosRecaudacion( TRUNC(PIFECHA), m.tblcodarg, vError);
      
      IF vError=1 THEN
      dbms_output.put_line('ERROR AL PROCESAR');
      ELSE
      COMMIT;
      END IF;
 
      FOR b IN banco LOOP 
      
       Pkg_RecaudacionEnvio.P_GEN_ARCHIVOTXT(b.tblcodarg,m.tblcodarg);
     
       END LOOP;
      
      --Globokasnet / no es un banco
      IF m.tblcodarg=1 THEN
        Pkg_RecaudacionEnvio.P_GEN_ARCHIVOTXT(16969,m.tblcodarg);
      End If;
END LOOP;

  --Proceso Especifico InterBank
  Pkg_RecaudacionEnvio.P_GEN_ARCHIVOTXT_IBK(); --Archivo TXT Bimoneda
  --
   
END P_GEN_GENERARARCHIVOS;

PROCEDURE P_GEN_ARCHIVOTXT_IBK IS
  v_archivo      UTL_FILE.FILE_TYPE;
  vDirectorio    VARCHAR2(500):='RECAUDABANCOS';
  vNombrearchivo VARCHAR2(500):=' ' ;
  vPlantillaInterbank VARCHAR2(300):='@VALOR1@@VALOR2@@VALOR3@@VALOR4@';

  vTotalImporteSoles    NUMBER(15,2);
  vTotalImporteDolares    NUMBER(15,2);
  vTotalRegistroSoles    NUMBER(10);
  vTotalRegistroDolares    NUMBER(10);

  --Datos IBK--
  --Tipos de Registo
  vCabeceraIBK        VARCHAR2(400);
  vCuotaIBK           VARCHAR2(400);
  vDetalleIBK         VARCHAR2(400);
  --

  vTipoCabeceraIBK       VARCHAR2(2) := '11';
  vCodGrupoIBK           VARCHAR2(2) := '21';
  vCodRubroIBK           VARCHAR2(2) := '03';
  vCodEmpresaIBK         VARCHAR2(3) := '953';
  vCodServicioIBK        VARCHAR2(2) := '01';
  vCodUnicoIBK           VARCHAR2(10) := '0005457247';

  vTipoCuotaIBK          VARCHAR2(2) := '12';

  vTipoDetalleIBK        VARCHAR2(2) := '13';
  --Fin Datos IBK--

  CURSOR cIBK IS
  SELECT CAMPO FROM RECAUDAINTERBANKSOLES WHERE TIPO = 2
  UNION ALL
  SELECT CAMPO FROM RECAUDAINTERBANKDOLARES WHERE TIPO = 2;

BEGIN
  SELECT CAMPO INTO vTotalImporteSoles FROM RECAUDAINTERBANKSOLES WHERE TIPO = 0;
  SELECT CAMPO INTO vTotalImporteDolares FROM RECAUDAINTERBANKDOLARES WHERE TIPO = 0;
  SELECT CAMPO INTO vTotalRegistroSoles FROM RECAUDAINTERBANKSOLES WHERE TIPO = 1;
  SELECT CAMPO INTO vTotalRegistroDolares FROM RECAUDAINTERBANKDOLARES WHERE TIPO = 1;
  
  vCabeceraIBK := vTipoCabeceraIBK ||                            --Tipo de registro
                  vCodGrupoIBK ||                                --Código de grupo
                  vCodRubroIBK ||                                --Código de rubro
                  vCodEmpresaIBK ||                              --Código de empresa
                  vCodServicioIBK ||                             --Código de servicio
                  '01' ||                                     --Código de solicitud
                  RPAD('PRESTAMO', 30, ' ') ||                --Descripción de solicitud
                  '0' ||                                      --Origen de la Solicitud
                  '002' ||                                    --Código de requerimiento
                  '1' ||                                      --Canal de envío
                  'M' ||                                      --Tipo de información --M: Reemplaza --A:Agrega
                  LPAD(vTotalRegistroSoles + vTotalRegistroDolares, 15, '0') ||          --Número de registros
                  vCodUnicoIBK ||                                --Código único
                  TO_CHAR(HOY, 'YYYYMMDD') ||                 --Fecha de proceso
                  '00000000' ||                               --Fecha de Inicio de Cargos
                  '00' ||                               --Moneda Solo aplica para Pago Automatico, caso contrario 00
                  LPAD(NVL(vTotalImporteSoles, 0), 15, '0') ||
                                                              --Suma Total Soles
                  LPAD(NVL(vTotalImporteDolares, 0), 15, '0') ||
                                                              --Suma Total Dolares
                  'P' ||                                      --Tipo de Glosa        
                  LPAD(' ', 50, ' ') ||                       --Glosa General
                  LPAD(' ', 221, ' ') ||                      --Glosa General
                  '02' ||                                     --Tipo Formato
                  '0000';                                     --Código fijo

  vCuotaIBK :=    vTipoCuotaIBK ||                            --Tipo de registro
                  '00000000' ||                               --Código de cuota
                  '1' ||                                      --Número de conceptos
                  RPAD(UPPER(TRIM(TO_CHAR(HOY, 'MONTH'))), 10, ' ') ||
                                                              --Descripción del concepto 1
                  LPAD(' ', 10,' ') ||                        --Descripción del concepto 2
                  LPAD(' ', 10,' ') ||                        --Descripción del concepto 3
                  LPAD(' ', 10,' ') ||                        --Descripción del concepto 4
                  LPAD(' ', 10,' ') ||                        --Descripción del concepto 5
                  LPAD(' ', 10,' ') ||                        --Descripción del concepto 6
                  LPAD(' ', 10,' ') ||                        --Descripción del concepto 7                
                  LPAD(' ', 313,' ') ||                       --Libre
                  '02' ||                                     --Tipo Formato
                  '0000';                                     --Código fijo
  
  vNombrearchivo :=REPLACE(vPlantillaInterbank, '@VALOR1@', 'C');
  vNombrearchivo :=REPLACE(vNombrearchivo, '@VALOR2@', '03');    ---Codigo asignado por el banco
  vNombrearchivo :=REPLACE(vNombrearchivo, '@VALOR3@', '953');   ---Codigo asignado por el banco
  vNombrearchivo :=REPLACE(vNombrearchivo, '@VALOR4@', '01');    ---Codigo asignado por el banco
  
  v_archivo := UTL_FILE.FOPEN(vDirectorio, vNombrearchivo||'.TXT','W');
  --DELETE FROM RECAUDAINTERBANK;
  COMMIT;

  UTL_FILE.PUT_LINE(v_archivo, vCabeceraIBK||CHR(13));
  --INSERT INTO RECAUDAINTERBANK (CAMPO) VALUES (vCabeceraIBK);
  UTL_FILE.PUT_LINE(v_archivo, vCuotaIBK||CHR(13));
  --INSERT INTO RECAUDAINTERBANK (CAMPO) VALUES (vCuotaIBK);

  FOR x IN cIBK LOOP 
    UTL_FILE.PUT_LINE(v_archivo, x.campo||CHR(13));
    --INSERT INTO RECAUDAINTERBANK (CAMPO) VALUES (x.campo);
  END LOOP;
  --COMMIT;
  UTL_FILE.FCLOSE(v_archivo);
EXCEPTION WHEN OTHERS THEN
  DBMS_OUTPUT.PUT_LINE('ERROR GENERAR TXT IBK BIMONEDA');
  UTL_FILE.FCLOSE(v_archivo);
END P_GEN_ARCHIVOTXT_IBK;
 ---
END PKG_RECAUDACIONENVIO;
/