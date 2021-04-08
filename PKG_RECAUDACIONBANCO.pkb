CREATE OR REPLACE PACKAGE BODY SISGODBA.PKG_RECAUDACIONBANCO IS
-- Log de cambios
-- 05.06.2019 - Juan Salazar C. - Creacion de package.
-- 27.08.2019 - Juan Salazar C. - Creacion de Procedure P_GEN_PROCESAPAGOS
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

PROCEDURE P_GEN_CARGABANCONACION ( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
                            PIFechaProceso IN DATE:=SYSDATE
                          ) IS

    in_file    UTL_FILE.FILE_TYPE;
    linebuf             VARCHAR2(1000);
    vRecauda            recaudacionbanco%ROWTYPE;
    vValidaTrama        NUMBER;
    cFechaProceso       DATE                := SYSDATE;
    vNumerocuota        prestamocuotas.numerocuota%TYPE;

    vOrdenCobro         VARCHAR(12);                        --Variable que retorna, contiene ACT/ATR - Fecha envio (YYYYMMDD)
    cIdentificador      VARCHAR(7)          := '0180100';   --Codigo Banco + Cliente

BEGIN
    in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
    
    LOOP
        UTL_FILE.GET_LINE (in_file, linebuf);
        
        linebuf := trim(linebuf);
        IF linebuf IS NOT NULL AND SUBSTR(linebuf, 1, LENGTH(cIdentificador)) = cIdentificador THEN
        
            SELECT COUNT(*)
            INTO vValidaTrama
            FROM recaudacionbanco
            WHERE REPLACE(TRIM(trama), ' ', '') =  REPLACE(TRIM(linebuf), ' ', '');

            IF vValidaTrama = 0 AND LENGTH(linebuf) > 100 THEN

                vRecauda.trama                  := linebuf;
                vRecauda.fechacarga             := cFechaProceso;
                vRecauda.usuariocarga           := USER;
                vRecauda.codigobanco            := 5;        -- Codigo Banco en Datosbanco -- Scotiabank

                BEGIN
                    vRecauda.periodosolicitud   := SUBSTR(linebuf, 2, 4);
                    vRecauda.numerosolicitud    := TO_NUMBER(SUBSTR(linebuf, 6, 7));

                    vRecauda.numerocuota        := SUBSTR(linebuf, 13, 4);

                    --Situacion de pago
                    ----SUBSTR(linebuf, 17, 2);

                    vRecauda.moneda             := SUBSTR(linebuf, 19, 1); --1:Soles 2:Dolares

                    vRecauda.codigosocio        := PKG_PERSONA.F_OBT_CIP(PKG_PRESTAMO.F_OBT_CODIGOPERSONA(vRecauda.numerosolicitud, vRecauda.periodosolicitud));

                    --Nombre Cliente Retorna
                    ----SUBSTR(linebuf, 20, 60);
                    ----PKG_PRESTAMO.F_OBT_CODIGOPERSONA(vRecauda.periodosolicitud, vRecauda.numerosolicitud)
                    vRecauda.nombrecliente      := PKG_PERSONA.F_OBT_NOMBRECOMPLETO(PKG_PRESTAMO.F_OBT_CODIGOPERSONA(vRecauda.numerosolicitud, vRecauda.periodosolicitud));

                    --Importe Cuota
                    vRecauda.importeorigen      := TO_NUMBER(SUBSTR(linebuf, 80, 15)) / 100;

                    vRecauda.fechavencimiento   := TO_DATE  (
                                                            SUBSTR(linebuf, 95, 4)||'-'||
                                                            SUBSTR(linebuf, 99, 2)||'-'||
                                                            SUBSTR(linebuf, 101, 2),
                                                            'YYYY-MM-DD'
                                                            );

                    --Indicador de la Tasa
                    ----SUBSTR(linebuf, 103, 1);

                    --Factor Mora
                    ----TO_NUMBER(SUBSTR(linebuf, 104, 15)) / 100;

                    --Factor Compensatorio
                    ----TO_NUMBER(SUBSTR(linebuf, 119, 15)) / 100;

                    --Importe Gastos
                    ----TO_NUMBER(SUBSTR(linebuf, 134, 15)) / 100;

                    --Cuenta Cliente
                    ----SUBSTR(linebuf, 149, 11);

                    --Orden Cobro. Variable que retorna, contiene ACT/ATR - Fecha envio (YYYYMMDD)
                    --vOrdenCobro                 := SUBSTR(linebuf, 160, 12);
                    vOrdenCobro                 := '00000' || SUBSTR(linebuf, 73, 7);

                    /*vRecauda.tipopago           := 'ATR';
                    vRecauda.fechaenvio         := TO_DATE   ('2021-03-26',
                                                            'YYYY-MM-DD'
                                                            );
                    */
                    vRecauda.tipopago           :=  CASE SUBSTR(vOrdenCobro, 6, 1)
                                                        WHEN 1 THEN
                                                            'ACT'
                                                        WHEN 2 THEN
                                                            'ATR'
                                                    END;
                    vRecauda.fechaenvio         :=  TO_DATE   (
                                                            SUBSTR(vOrdenCobro, 7, 2)||'-'||
                                                            SUBSTR(vOrdenCobro, 9, 2)||'-'||
                                                            SUBSTR(vOrdenCobro, 11, 2),
                                                            'YY-MM-DD'
                                                            );

                    --Mora
                    ----TO_NUMBER(SUBSTR(linebuf, 172, 15)) / 100;
                    vRecauda.importemora        := 0;

                    --Compensacion
                    ----TO_NUMBER(SUBSTR(linebuf, 187, 15)) / 100;

                    --Importe Cobrado
                    vRecauda.importedepositado  := TO_NUMBER(SUBSTR(linebuf, 202, 15)) / 100;

                    --Agencia de cobro
                    ----SUBSTR(linebuf, 217, 4);
                    vRecauda.oficinapago        := SUBSTR(linebuf, 217, 4);

                    vRecauda.fechapago          := TO_DATE  (
                                                            SUBSTR(linebuf, 221, 4) || '-' ||
                                                            SUBSTR(linebuf, 225, 2) || '-' ||
                                                            SUBSTR(linebuf, 227, 2)
                                                            , 'YYYY-MM-DD'
                                                            );
                    
                    --Hora Cobro HHMMSS
                    vRecauda.nromovimiento      := SUBSTR(linebuf, 229, 6);

                    vRecauda.referencias        := vRecauda.codigosocio||vRecauda.tipopago||vRecauda.nromovimiento;

                    --Espacios Vacios
                    ----SUBSTR(linebuf, 235, 60);

                    vRecauda.numerocuentabanco  := PKG_DATOSBANCO.F_OBT_CUENTABANCORECAUDA(vRecauda.codigobanco, vRecauda.moneda);

                    BEGIN
                        PKG_RECAUDACIONBANCO.P_OBT_VERIFICARDEBITOAUTO(vRecauda.periodosolicitud, vRecauda.numerosolicitud, vRecauda.debitoautomatico);
                        vRecauda.estado := '1';
                    EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20120,'  vRecauda.estado  ' || vRecauda.estado  );
                    END;
                    
                    IF vRecauda.tipopago = 'ACT' THEN
                        BEGIN
                            SELECT MIN(numerocuota)
                                INTO vNumerocuota
                                FROM prestamocuotas 
                            WHERE periodosolicitud = vRecauda.periodosolicitud 
                                AND numerosolicitud =  vRecauda.numerosolicitud 
                                AND estado = 2;
                        EXCEPTION WHEN OTHERS THEN
                           vNumerocuota := NULL;       
                    END; 
                    --
                    vRecauda.cuotacronograma := vNumerocuota;
                    --
                    vRecauda.amortizacion   := PKG_PRESTAMOCUOTAS.F_OBT_AMORTIZACION(   vRecauda.numerosolicitud, 
                                                                                        vRecauda.periodosolicitud, 
                                                                                        vNumerocuota);
                    --
                    vRecauda.interes        := PKG_PRESTAMOCUOTAS.F_OBT_INTERES(    vRecauda.numerosolicitud,
                                                                                    vRecauda.periodosolicitud, 
                                                                                    vNumerocuota );
                    vRecauda.mora           := 0;
                    -- 
                    vRecauda.reajuste       := PKG_PRESTAMOCUOTAS.F_OBT_REAJUSTE(   vRecauda.numerosolicitud, 
                                                                                    vRecauda.periodosolicitud, 
                                                                                    vNumerocuota);
                    -- 
                    vRecauda.portes         := PKG_PRESTAMOCUOTAS.F_OBT_PORTES(     vRecauda.numerosolicitud, 
                                                                                    vRecauda.periodosolicitud, 
                                                                                    vNumerocuota);
                    --
                    vRecauda.segurointeres  := PKG_PRESTAMOCUOTAS.F_OBT_SEGUROINTERES(  vRecauda.numerosolicitud, 
                                                                                        vRecauda.periodosolicitud, 
                                                                                        vNumerocuota); 
                     
                    ELSIF vRecauda.tipopago = 'ATR' THEN
                            SELECT SUM(SALDOCAPITAL)
                            INTO vRecauda.amortizacion
                            FROM TABLE( CRE08070.DEUDACUOTASSIP(    vRecauda.periodosolicitud, 
                                                                    vRecauda.numerosolicitud, 
                                                                    vRecauda.fechaenvio ) ) 
                            WHERE FECHAVENCIMIENTO <= vRecauda.fechaenvio
                            AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                            --                                                         
                            vRecauda.interes         := PKG_RECAUDACIONBANCO.F_OBT_SALDOINTERES(    vRecauda.periodosolicitud, 
                                                                                                    vRecauda.numerosolicitud, 
                                                                                                    vRecauda.fechaenvio );
                            --
                            vRecauda.mora            := PKG_RECAUDACIONBANCO.F_OBT_SALDOMORA(   vRecauda.periodosolicitud, 
                                                                                                vRecauda.numerosolicitud, 
                                                                                                vRecauda.fechaenvio );
                            --
                            vRecauda.reajuste       := PKG_RECAUDACIONBANCO.F_OBT_SALDOREAJUSTE(    vRecauda.periodosolicitud, 
                                                                                                    vRecauda.numerosolicitud, 
                                                                                                    vRecauda.fechaenvio );
                            -- 
                            vRecauda.portes         := PKG_RECAUDACIONBANCO.F_OBT_SALDOPORTES(  vRecauda.periodosolicitud, 
                                                                                                vRecauda.numerosolicitud, 
                                                                                                vRecauda.fechaenvio );
                            --
                            vRecauda.segurointeres  := PKG_RECAUDACIONBANCO.F_OBT_SALDOSEGUROINTERES(   vRecauda.periodosolicitud, 
                                                                                                        vRecauda.numerosolicitud, 
                                                                                                        vRecauda.fechaenvio );
                    END IF;

                    vRecauda.totalcuota         :=  NVL(vRecauda.amortizacion, 0) +
                                                    NVL(vRecauda.interes, 0) +
                                                    NVL(vRecauda.mora, 0) +
                                                    NVL(vRecauda.reajuste, 0) +
                                                    NVL(vRecauda.portes, 0) +
                                                    NVL(vRecauda.segurointeres, 0);
                    --
                    IF vRecauda.numerocuota <> vRecauda.cuotacronograma THEN
                        vRecauda.observaciones     := vRecauda.observaciones || ' CUOTAS DIFERENTES ' || CHR(9);
                    END IF;

                    IF vRecauda.importeorigen <> vRecauda.totalcuota THEN
                        vRecauda.observaciones     := vRecauda.observaciones || ' IMPORTES DIFERENTES ' || CHR(9);
                    END IF;

                    BEGIN
                        INSERT INTO recaudacionbanco(   fechacarga,
                                                        usuariocarga,
                                                        codigosocio,
                                                        nombrecliente,
                                                        referencias,
                                                        importeorigen,
                                                        importedepositado,
                                                        importemora,
                                                        oficinapago,
                                                        nromovimiento,
                                                        fechapago,
                                                        tipopago,
                                                        estado,
                                                        codigobanco,
                                                        numerocuentabanco,
                                                        periodosolicitud,
                                                        numerosolicitud,
                                                        moneda,
                                                        numerocuota,
                                                        fechavencimiento,
                                                        amortizacion,
                                                        interes,
                                                        mora,
                                                        reajuste,
                                                        portes,
                                                        segurointeres,
                                                        fechaproceso,
                                                        usuarioproceso,
                                                        trama,
                                                        fechaenvio,
                                                        debitoautomatico,
                                                        cuotacronograma,
                                                        totalcuota,
                                                        observaciones
                                                    )
                            VALUES (    vRecauda.fechacarga,
                                        vRecauda.usuariocarga,
                                        vRecauda.codigosocio,
                                        vRecauda.nombrecliente,
                                        vRecauda.referencias,
                                        vRecauda.importeorigen,
                                        vRecauda.importedepositado,
                                        vRecauda.importemora,
                                        vRecauda.oficinapago,
                                        vRecauda.nromovimiento,
                                        vRecauda.fechapago,
                                        vRecauda.tipopago,
                                        vRecauda.estado,
                                        vRecauda.codigobanco,
                                        vRecauda.numerocuentabanco,
                                        vRecauda.periodosolicitud,
                                        vRecauda.numerosolicitud,
                                        vRecauda.moneda,
                                        vRecauda.numerocuota,
                                        vRecauda.fechavencimiento,
                                        vRecauda.amortizacion,
                                        vRecauda.interes,
                                        vRecauda.mora,
                                        vRecauda.reajuste,
                                        vRecauda.portes,
                                        vRecauda.segurointeres,
                                        vRecauda.fechaproceso,
                                        vRecauda.usuarioproceso,
                                        vRecauda.trama,
                                        vRecauda.fechaenvio,
                                        vRecauda.debitoautomatico,
                                        vRecauda.cuotacronograma,
                                        vRecauda.totalcuota,
                                        vRecauda.observaciones
                                    );
                        COMMIT;
                    END;
                END;
            END IF;
        END IF;
    END LOOP;

  EXCEPTION WHEN NO_DATA_FOUND THEN
      UTL_FILE.FCLOSE (in_file);
  WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20300,'Error CargaLiquidacion: '||SQLERRM);

  UTL_FILE.FCLOSE (in_file);
END P_GEN_CARGABANCONACION;

PROCEDURE P_GEN_CARGAINTERBANK( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
                            PIFechaProceso IN DATE:=SYSDATE
                          ) IS
    in_file    UTL_FILE.FILE_TYPE;
  linebuf       VARCHAR2 (1000);
  cRecauda      recaudacionbanco%ROWTYPE;
  vFechapago      VARCHAR2(8);
  vValidaTrama    NUMBER;
  vFechaProceso   DATE:= SYSDATE;
  vNumerocuota    prestamocuotas.numerocuota%TYPE;

  vRubro        VARCHAR(2) := '03';
  vCodEmpresa     VARCHAR(3) := '953';
  vCodServicio    VARCHAR(2) := '01';

  vPeriodoSolicitud   VARCHAR(4);
  vNumeroSolicitud    VARCHAR(7);

BEGIN
    
    in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
    
    LOOP
        UTL_FILE.GET_LINE (in_file, linebuf);
        
        linebuf := trim(linebuf);

        IF linebuf IS NOT NULL AND SUBSTR(linebuf, 1, 7) = vRubro || vCodEmpresa || vCodServicio THEN
          SELECT COUNT(*)
          INTO vValidaTrama
          FROM RECAUDACIONBANCO
          WHERE REPLACE(TRIM(TRAMA), ' ', '') = REPLACE(TRIM(linebuf), ' ', '');

          IF vValidaTrama = 0 THEN

            --Moneda
            IF SUBSTR(linebuf, 8, 2) = '01' THEN
              cRecauda.moneda := 1;
            ELSE
              cRecauda.moneda := 2;
            END IF;

            cRecauda.trama          := linebuf;
            cRecauda.fechacarga       := vFechaProceso;
            cRecauda.usuariocarga     := USER;
            cRecauda.codigobanco      := 4;   -- Codigo Banco en Datosbanco

            BEGIN
              cRecauda.periodosolicitud := SUBSTR(linebuf, 46, 4);
              cRecauda.numerosolicitud  := SUBSTR(linebuf, 30, 7);

              cRecauda.numerocuota    := SUBSTR(linebuf, 38, 3);

              cRecauda.nombrecliente    := TRIM(SUBSTR(linebuf, 53, 30));

              cRecauda.tipopago     := SUBSTR(linebuf, 42, 3);
                    
              cRecauda.referencias        := SUBSTR(linebuf, 83, 14);

              cRecauda.numerocuentabanco  := pkg_datosbanco.f_obt_cuentabancorecauda(cRecauda.codigobanco, cRecauda.moneda);
                    
              SELECT b.CIP INTO cRecauda.codigosocio
              FROM PRESTAMO a,PERSONA b
              WHERE a.PERIODOSOLICITUD=cRecauda.periodosolicitud
                AND a.NUMEROSOLICITUD=cRecauda.numerosolicitud
                AND b.CODIGOPERSONA=a.CODIGOPERSONA
                AND ROWNUM = 1;

              cRecauda.importeorigen    := TO_NUMBER(LTRIM(SUBSTR(linebuf, 97, 13), '0')) / 100;
              cRecauda.importedepositado  := cRecauda.importeorigen;

              cRecauda.importemora    := TO_NUMBER(SUBSTR(linebuf, 110, 7));
              
              cRecauda.oficinapago    := 0;

              cRecauda.nromovimiento    := SUBSTR(linebuf, 140, 8);

              cRecauda.fechaenvio     := TO_DATE  (
                                  SUBSTR(linebuf, 130, 2)||'/'||
                                  SUBSTR(linebuf, 128, 2)||'/'||
                                  SUBSTR(linebuf, 124, 4),
                                  'DD/MM/RR'
                                  );

              cRecauda.fechavencimiento   := TO_DATE  (
                                  SUBSTR(linebuf, 138, 2)||'/'||
                                  SUBSTR(linebuf, 136, 2)||'/'||
                                  SUBSTR(linebuf, 132, 4),
                                  'DD/MM/RR'
                                  );

              cRecauda.fechapago      := TO_DATE  (
                                  SUBSTR(linebuf, 89, 2)||'/'||
                                  SUBSTR(linebuf, 87, 2)||'/'||
                                  SUBSTR(linebuf, 83, 4),
                                  'DD/MM/RR'
                                  );

              --cRecauda.fechaproceso     := TO_DATE(SUBSTR(linebuf, 105, 2) || '/' || SUBSTR(linebuf, 103, 2) || '/' || SUBSTR(linebuf, 99, 4), 'DD/MM/RRRR');
              --cRecauda.usuarioproceso   := SUBSTR(linebuf, 91, 8);
              BEGIN
                SELECT MIN(numerocuota)
                INTO vNumerocuota
                FROM prestamocuotas 
                WHERE periodosolicitud = cRecauda.periodosolicitud 
                AND numerosolicitud = cRecauda.numerosolicitud 
                AND estado = 2;
              EXCEPTION WHEN OTHERS THEN
                vNumerocuota := NULL;
              END; 

              BEGIN
                PKG_RECAUDACIONBANCO.P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud, cRecauda.numerosolicitud, cRecauda.debitoautomatico);
                cRecauda.estado := '1';
              EXCEPTION WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
              END;
              IF cRecauda.tipopago = 'ACT' THEN
                BEGIN
                  SELECT MIN(numerocuota)
                    INTO vNumerocuota
                    FROM prestamocuotas
                  WHERE periodosolicitud = cRecauda.periodosolicitud 
                    AND numerosolicitud =  cRecauda.numerosolicitud 
                    AND estado = 2;
                EXCEPTION WHEN OTHERS THEN
                  vNumerocuota := NULL;
                END; 
                --
                cRecauda.cuotacronograma := vNumerocuota;
                --
                cRecauda.amortizacion   := PKG_PRESTAMOCUOTAS.F_OBT_AMORTIZACION(   cRecauda.numerosolicitud, 
                                                                                    cRecauda.periodosolicitud, 
                                                                                    vNumerocuota);
                --
                cRecauda.interes        := PKG_PRESTAMOCUOTAS.F_OBT_INTERES(    cRecauda.numerosolicitud,
                                                                                cRecauda.periodosolicitud, 
                                                                                vNumerocuota );
                cRecauda.mora           := 0;
                -- 
                cRecauda.reajuste       := PKG_PRESTAMOCUOTAS.F_OBT_REAJUSTE(   cRecauda.numerosolicitud, 
                                                                                cRecauda.periodosolicitud, 
                                                                                vNumerocuota);
                -- 
                cRecauda.portes         := PKG_PRESTAMOCUOTAS.F_OBT_PORTES(     cRecauda.numerosolicitud, 
                                                                                cRecauda.periodosolicitud, 
                                                                                vNumerocuota);
                --
                cRecauda.segurointeres  := PKG_PRESTAMOCUOTAS.F_OBT_SEGUROINTERES(  cRecauda.numerosolicitud, 
                                                                                    cRecauda.periodosolicitud, 
                                                                                    vNumerocuota); 
                  
              ELSIF cRecauda.tipopago = 'ATR' THEN
                SELECT SUM(SALDOCAPITAL)
                INTO cRecauda.amortizacion
                FROM TABLE( CRE08070.DEUDACUOTASSIP(    cRecauda.periodosolicitud, 
                                                        cRecauda.numerosolicitud, 
                                                        cRecauda.fechaenvio ) ) 
                WHERE FECHAVENCIMIENTO <= cRecauda.fechaenvio
                AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                --                                                         
                cRecauda.interes         := PKG_RECAUDACIONBANCO.F_OBT_SALDOINTERES(    cRecauda.periodosolicitud, 
                                                                                        cRecauda.numerosolicitud, 
                                                                                        cRecauda.fechaenvio );
                --
                cRecauda.mora            := PKG_RECAUDACIONBANCO.F_OBT_SALDOMORA(   cRecauda.periodosolicitud, 
                                                                                    cRecauda.numerosolicitud, 
                                                                                    cRecauda.fechaenvio );
                --
                cRecauda.reajuste       := PKG_RECAUDACIONBANCO.F_OBT_SALDOREAJUSTE(    cRecauda.periodosolicitud, 
                                                                                        cRecauda.numerosolicitud, 
                                                                                        cRecauda.fechaenvio );
                -- 
                cRecauda.portes         := PKG_RECAUDACIONBANCO.F_OBT_SALDOPORTES(  cRecauda.periodosolicitud, 
                                                                                    cRecauda.numerosolicitud, 
                                                                                    cRecauda.fechaenvio );
                --
                cRecauda.segurointeres  := PKG_RECAUDACIONBANCO.F_OBT_SALDOSEGUROINTERES(   cRecauda.periodosolicitud, 
                                                                                            cRecauda.numerosolicitud, 
                                                                                            cRecauda.fechaenvio );
              END IF;

              cRecauda.totalcuota     :=  NVL(cRecauda.amortizacion, 0) +
                              NVL(cRecauda.interes, 0) +
                              NVL(cRecauda.mora, 0) +
                              NVL(cRecauda.reajuste, 0) +
                              NVL(cRecauda.portes, 0) +
                              NVL(cRecauda.segurointeres, 0);
              --
              IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
                cRecauda.observaciones  := cRecauda.observaciones || ' CUOTAS DIFERENTES ' || CHR(9);
              END IF;

              IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                cRecauda.observaciones  := cRecauda.observaciones || ' IMPORTES DIFERENTES ' || CHR(9);
              END IF;

              BEGIN
                INSERT INTO recaudacionbanco( fechacarga,
                usuariocarga,
                codigosocio,
                nombrecliente,
                referencias,
                importeorigen,
                importedepositado,
                importemora,
                oficinapago,
                nromovimiento,
                fechapago,
                tipopago,
                estado,
                codigobanco,
                numerocuentabanco,
                periodosolicitud,
                numerosolicitud,
                moneda,
                numerocuota,
                fechavencimiento,
                amortizacion,
                interes,
                mora,
                reajuste,
                portes,
                segurointeres,
                fechaproceso,
                usuarioproceso,
                trama,
                fechaenvio,
                debitoautomatico,
                cuotacronograma,
                totalcuota,
                observaciones
                )
                VALUES ( cRecauda.fechacarga,
                cRecauda.usuariocarga,
                cRecauda.codigosocio,
                cRecauda.nombrecliente,
                cRecauda.referencias,
                cRecauda.importeorigen,
                cRecauda.importedepositado,
                cRecauda.importemora,
                cRecauda.oficinapago,
                cRecauda.nromovimiento,
                cRecauda.fechapago,
                cRecauda.tipopago,
                cRecauda.estado,
                cRecauda.codigobanco,
                cRecauda.numerocuentabanco,
                cRecauda.periodosolicitud,
                cRecauda.numerosolicitud,
                cRecauda.moneda,
                cRecauda.numerocuota,
                cRecauda.fechavencimiento,
                cRecauda.amortizacion,
                cRecauda.interes,
                cRecauda.mora,
                cRecauda.reajuste,
                cRecauda.portes,
                cRecauda.segurointeres,
                cRecauda.fechaproceso,
                cRecauda.usuarioproceso,
                cRecauda.trama,
                cRecauda.fechaenvio,
                cRecauda.debitoautomatico,
                cRecauda.cuotacronograma,
                cRecauda.totalcuota,
                cRecauda.observaciones
                ) ;
                COMMIT;
              END;
            END;
          END IF;
        END IF;
    END LOOP;

EXCEPTION WHEN NO_DATA_FOUND THEN
    UTL_FILE.FCLOSE (in_file);
WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20300,'Error CargaLiquidacion: '||SQLERRM);

UTL_FILE.FCLOSE (in_file);

END P_GEN_CARGAINTERBANK;

PROCEDURE P_GEN_CARGAGLOBOKAS( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
                            PIFechaProceso IN DATE:=SYSDATE
                          ) IS
  in_file         UTL_FILE.FILE_TYPE;
	linebuf				  VARCHAR2 (1000);
	cRecauda			  recaudacionbanco%ROWTYPE;
	vValidaTrama		NUMBER;
	vFechaProceso		DATE:= SYSDATE;
	vNumerocuota		prestamocuotas.numerocuota%TYPE;

	vCodSVC				VARCHAR(4) := '3300'; --Codigo del Servicio SVC
BEGIN
    
    in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
    
    LOOP
        UTL_FILE.GET_LINE (in_file, linebuf);
        
        linebuf := trim(linebuf);

        IF linebuf IS NOT NULL AND SUBSTR(linebuf, 1, 4) = vCodSVC THEN
            SELECT COUNT(*) INTO vValidaTrama 
            FROM RECAUDACIONBANCO 
            WHERE REPLACE(TRIM(TRAMA),' ','')= REPLACE(TRIM(linebuf),' ','');
            
            IF vValidaTrama = 0 THEN

                cRecauda.trama					  := linebuf;
                cRecauda.fechacarga				:= vFechaProceso;
                cRecauda.usuariocarga			:= USER;
                cRecauda.codigobanco			:= 5;		-- Codigo Banco en Datosbanco -- ScotiaBank

                BEGIN
                  --CODSER Siempre 33
                  --SUBSTR(linebuf, 1, 2)

                  --CODSEC Siempre 00
                  --SUBSTR(linebuf, 3, 2)

                  --NROFAC (PERIODO+SOLICITUD-ACT/ATR)
                  --SUBSTR(linebuf, 5, 15)
                  cRecauda.periodosolicitud  	:= SUBSTR(linebuf, 5, 4);
                  cRecauda.numerosolicitud   	:= SUBSTR(linebuf, 9, 7);
                  --guion
                  cRecauda.tipopago			:= SUBSTR(linebuf, 17, 3);

                  SELECT b.CIP, b.nombrecompleto INTO cRecauda.codigosocio, cRecauda.nombrecliente
                  FROM PRESTAMO a,PERSONA b
                  WHERE a.PERIODOSOLICITUD=cRecauda.periodosolicitud
                    AND a.NUMEROSOLICITUD=cRecauda.numerosolicitud
                    AND b.CODIGOPERSONA=a.CODIGOPERSONA
                    AND ROWNUM = 1;

                  --FECFAC (YYYYMMDD)
                  --SUBSTR(linebuf, 20, 8)

                  --MONFAC (Soles = 1)
                  --SUBSTR(linebuf, 28, 1)
                  cRecauda.moneda           	:= SUBSTR(linebuf, 28, 1);

                  --NROSER (Cod. Socio) 
                  --SUBSTR(linebuf, 29, 15)

                  --NROCLI (NroCuota, FechaVencimiento, FechaEnvio) 
                  --SUBSTR(linebuf, 44, 15)
                  cRecauda.numerocuota		:= SUBSTR(linebuf, 44, 3);
                  cRecauda.fechavencimiento 	:= TO_DATE 	(
                                      SUBSTR(linebuf, 47, 2)||'-'||
                                      SUBSTR(linebuf, 49, 2)||'-'||
                                      SUBSTR(linebuf, 51, 2),
                                      'YY-MM-DD'
                                      );
                  cRecauda.fechaenvio 		:= TO_DATE 	(
                                      SUBSTR(linebuf, 53, 2)||'-'||
                                      SUBSTR(linebuf, 55, 2)||'-'||
                                      SUBSTR(linebuf, 57, 2),
                                      'YY-MM-DD'
                                      );

                  cRecauda.nromovimiento 		:= SUBSTR(linebuf, 44, 15);

                  --NROCEN Siempre 000
                  --SUBSTR(linebuf, 59, 3)

                  --IMPTOT
                  --SUBSTR(linebuf, 62, 12)
                  cRecauda.importeorigen 		:= TO_NUMBER(SUBSTR(linebuf, 62, 12)) / 100;
                  cRecauda.importedepositado 	:= cRecauda.importeorigen;
                  
                  --TIPODOC1 Siempre R
                  --SUBSTR(linebuf, 74, 2)
                  
                  --TIPOENT Siempre CT
                  --SUBSTR(linebuf, 76, 2)
                  
                  --CODENT
                  --SUBSTR(linebuf, 78, 4)
                  
                  --CODSUC
                  --SUBSTR(linebuf, 82, 6)
                  
                  --CODAGE
                  --SUBSTR(linebuf, 88, 3)
                  
                  --USUARIO
                  --SUBSTR(linebuf, 91, 8)
                  cRecauda.referencias      	:= SUBSTR(linebuf, 74, 25);
                  
                  --FECCAN (YYYYMMDD)
                  --SUBSTR(linebuf, 99, 8)
                  cRecauda.fechapago 			:= TO_DATE 	(
                                      SUBSTR(linebuf, 99, 4)||'-'||
                                      SUBSTR(linebuf, 103, 2)||'-'||
                                      SUBSTR(linebuf, 105, 2),
                                      'YYYY-MM-DD'
                                      );
                  
                  --HORPAG (HHMMSS)
                  --SUBSTR(linebuf, 107, 6)
                  
                  --CODNEG
                  --SUBSTR(linebuf, 113, 2)
                  
                  --VIAPAG
                  --SUBSTR(linebuf, 115, 2)
                  
                  --FILLER
                  --SUBSTR(linebuf, 117, 15)

                  cRecauda.numerocuentabanco 	:= pkg_datosbanco.f_obt_cuentabancorecauda(cRecauda.codigobanco, cRecauda.moneda);

                  cRecauda.importemora 		:= 0;
                  cRecauda.oficinapago 		:= 0;

                  BEGIN
                    SELECT MIN(numerocuota)
                    INTO vNumerocuota
                    FROM prestamocuotas 
                    WHERE periodosolicitud = cRecauda.periodosolicitud 
                    AND numerosolicitud = cRecauda.numerosolicitud 
                    AND estado = 2;
                  EXCEPTION WHEN OTHERS THEN
                    vNumerocuota := NULL;
                  END; 

                  BEGIN
                    PKG_RECAUDACIONBANCO.P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud, cRecauda.numerosolicitud, cRecauda.debitoautomatico);
                    cRecauda.estado := '1';
                  EXCEPTION WHEN OTHERS THEN
                    RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
                  END;

                  IF cRecauda.tipopago = 'ACT' THEN
                    BEGIN
                      SELECT MIN(numerocuota)
                        INTO vNumerocuota
                        FROM prestamocuotas
                      WHERE periodosolicitud = cRecauda.periodosolicitud 
                        AND numerosolicitud =  cRecauda.numerosolicitud 
                        AND estado = 2;
                    EXCEPTION WHEN OTHERS THEN
                      vNumerocuota := NULL;
                    END; 
                    --
                    cRecauda.cuotacronograma := vNumerocuota;
                    --
                    cRecauda.amortizacion   := PKG_PRESTAMOCUOTAS.F_OBT_AMORTIZACION(   cRecauda.numerosolicitud, 
                                                                                        cRecauda.periodosolicitud, 
                                                                                        vNumerocuota);
                    --
                    cRecauda.interes        := PKG_PRESTAMOCUOTAS.F_OBT_INTERES(    cRecauda.numerosolicitud,
                                                                                    cRecauda.periodosolicitud, 
                                                                                    vNumerocuota );
                    cRecauda.mora           := 0;
                    -- 
                    cRecauda.reajuste       := PKG_PRESTAMOCUOTAS.F_OBT_REAJUSTE(   cRecauda.numerosolicitud, 
                                                                                    cRecauda.periodosolicitud, 
                                                                                    vNumerocuota);
                    -- 
                    cRecauda.portes         := PKG_PRESTAMOCUOTAS.F_OBT_PORTES(     cRecauda.numerosolicitud, 
                                                                                    cRecauda.periodosolicitud, 
                                                                                    vNumerocuota);
                    --
                    cRecauda.segurointeres  := PKG_PRESTAMOCUOTAS.F_OBT_SEGUROINTERES(  cRecauda.numerosolicitud, 
                                                                                        cRecauda.periodosolicitud, 
                                                                                        vNumerocuota); 
                      
                  ELSIF cRecauda.tipopago = 'ATR' THEN
                    SELECT SUM(SALDOCAPITAL)
                    INTO cRecauda.amortizacion
                    FROM TABLE( CRE08070.DEUDACUOTASSIP(    cRecauda.periodosolicitud, 
                                                            cRecauda.numerosolicitud, 
                                                            cRecauda.fechaenvio ) ) 
                    WHERE FECHAVENCIMIENTO <= cRecauda.fechaenvio
                    AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                    --                                                         
                    cRecauda.interes         := PKG_RECAUDACIONBANCO.F_OBT_SALDOINTERES(    cRecauda.periodosolicitud, 
                                                                                            cRecauda.numerosolicitud, 
                                                                                            cRecauda.fechaenvio );
                    --
                    cRecauda.mora            := PKG_RECAUDACIONBANCO.F_OBT_SALDOMORA(   cRecauda.periodosolicitud, 
                                                                                        cRecauda.numerosolicitud, 
                                                                                        cRecauda.fechaenvio );
                    --
                    cRecauda.reajuste       := PKG_RECAUDACIONBANCO.F_OBT_SALDOREAJUSTE(    cRecauda.periodosolicitud, 
                                                                                            cRecauda.numerosolicitud, 
                                                                                            cRecauda.fechaenvio );
                    -- 
                    cRecauda.portes         := PKG_RECAUDACIONBANCO.F_OBT_SALDOPORTES(  cRecauda.periodosolicitud, 
                                                                                        cRecauda.numerosolicitud, 
                                                                                        cRecauda.fechaenvio );
                    --
                    cRecauda.segurointeres  := PKG_RECAUDACIONBANCO.F_OBT_SALDOSEGUROINTERES(   cRecauda.periodosolicitud, 
                                                                                                cRecauda.numerosolicitud, 
                                                                                                cRecauda.fechaenvio );
                  END IF;
                  cRecauda.totalcuota 		  := 	NVL(cRecauda.amortizacion, 0) +
                                                NVL(cRecauda.interes, 0) +
                                                NVL(cRecauda.mora, 0) +
                                                NVL(cRecauda.reajuste, 0) +
                                                NVL(cRecauda.portes, 0) +
                                                NVL(cRecauda.segurointeres, 0);
                  --
                  IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
                    cRecauda.observaciones 	:= cRecauda.observaciones || ' CUOTAS DIFERENTES ' || CHR(9);
                  END IF;

                  IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                    cRecauda.observaciones 	:= cRecauda.observaciones || ' IMPORTES DIFERENTES ' || CHR(9);
                  END IF;

                  BEGIN
                    INSERT INTO recaudacionbanco( fechacarga,
                    usuariocarga,
                    codigosocio,
                    nombrecliente,
                    referencias,
                    importeorigen,
                    importedepositado,
                    importemora,
                    oficinapago,
                    nromovimiento,
                    fechapago,
                    tipopago,
                    estado,
                    codigobanco,
                    numerocuentabanco,
                    periodosolicitud,
                    numerosolicitud,
                    moneda,
                    numerocuota,
                    fechavencimiento,
                    amortizacion,
                    interes,
                    mora,
                    reajuste,
                    portes,
                    segurointeres,
                    fechaproceso,
                    usuarioproceso,
                    trama,
                    fechaenvio,
                    debitoautomatico,
                    cuotacronograma,
                    totalcuota,
                    observaciones
                    )
                    VALUES ( cRecauda.fechacarga,
                    cRecauda.usuariocarga,
                    cRecauda.codigosocio,
                    cRecauda.nombrecliente,
                    cRecauda.referencias,
                    cRecauda.importeorigen,
                    cRecauda.importedepositado,
                    cRecauda.importemora,
                    cRecauda.oficinapago,
                    cRecauda.nromovimiento,
                    cRecauda.fechapago,
                    cRecauda.tipopago,
                    cRecauda.estado,
                    cRecauda.codigobanco,
                    cRecauda.numerocuentabanco,
                    cRecauda.periodosolicitud,
                    cRecauda.numerosolicitud,
                    cRecauda.moneda,
                    cRecauda.numerocuota,
                    cRecauda.fechavencimiento,
                    cRecauda.amortizacion,
                    cRecauda.interes,
                    cRecauda.mora,
                    cRecauda.reajuste,
                    cRecauda.portes,
                    cRecauda.segurointeres,
                    cRecauda.fechaproceso,
                    cRecauda.usuarioproceso,
                    cRecauda.trama,
                    cRecauda.fechaenvio,
                    cRecauda.debitoautomatico,
                    cRecauda.cuotacronograma,
                    cRecauda.totalcuota,
                    cRecauda.observaciones
                    ) ;
                    COMMIT;
                  END;
                END;
            END IF;
        END IF;
    END LOOP;

EXCEPTION WHEN NO_DATA_FOUND THEN
    UTL_FILE.FCLOSE (in_file);
WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20300,'Error CargaLiquidacion: '||SQLERRM);

UTL_FILE.FCLOSE (in_file);

END P_GEN_CARGAGLOBOKAS;

PROCEDURE P_GEN_CARGACONTI( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
                            PIFechaProceso IN DATE:=SYSDATE
                          ) IS
  in_file    UTL_FILE.FILE_TYPE;
  linebuf    VARCHAR2 (1000);
  cRecauda   recaudacionbanco%ROWTYPE;
  vFechapago VARCHAR2(8);
  vFechaenvio VARCHAR2(8);
  vAnocuota  VARCHAR2(4);
  vValidaTrama  NUMBER;
  vFechaProceso  DATE:= SYSDATE;
  vNumerocuota  prestamocuotas.numerocuota%TYPE;
BEGIN                    
      --
      in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
      --
      LOOP
          UTL_FILE.GET_LINE (in_file, linebuf);
          --
          linebuf := trim(linebuf);

          IF linebuf IS NOT NULL AND substr(linebuf,1,2 ) = '02' THEN
               
             SELECT COUNT(*) 
               INTO vValidaTrama 
               FROM RECAUDACIONBANCO 
              WHERE REPLACE(TRIM(TRAMA),' ','')= REPLACE(TRIM(linebuf),' ','');
             --   
            IF vValidaTrama = 0 THEN

              cRecauda.trama := linebuf;
              -- 02.01.2020 - David Chara Inca.
              --cRecauda.fechacarga   := SYSDATE;
              cRecauda.fechacarga   := vFechaProceso;
              cRecauda.usuariocarga := USER;
              cRecauda.codigobanco  := 6; -- Codigo Banco en Datosbanco
              --
              BEGIN
                   cRecauda.nombrecliente    := SUBSTR(linebuf, 3, 30 );
                   cRecauda.numerocuota      := SUBSTR(SUBSTR(TRIM(cRecauda.nombrecliente),-7),1,3);
                   -- 02.01.2020 - David Chara Inca.
                   --vAnocuota := TO_CHAR(SYSDATE,'RRRR');
                   vAnocuota := TO_CHAR(vFechaProceso,'RRRR');
                   cRecauda.fechavencimiento := TO_DATE (
                                                SUBSTR(SUBSTR(TRIM(SUBSTR(linebuf, 3, 30 )),-4),3,2)||'/'||
                                                SUBSTR(SUBSTR(TRIM(SUBSTR(linebuf, 3, 30 )),-4),1,2)||'/'||
                                                vAnocuota,
                                                'DD/MM/RRRR');
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20010,' cRecauda.nombrecliente ' ||cRecauda.nombrecliente);
              END;
              --
              BEGIN
                   cRecauda.referencias      := SUBSTR(linebuf, 33, 48 );
                   cRecauda.tipopago         := SUBSTR(TRIM(cRecauda.referencias),-9,3);
                   IF SUBSTR(TRIM(cRecauda.referencias),11,1) = 'S' THEN
                      cRecauda.moneda           := 1;
                   ELSE
                      cRecauda.moneda           := 2;
                   END IF;
                   cRecauda.numerocuentabanco := pkg_datosbanco.f_obt_cuentabancorecauda( cRecauda.codigobanco, cRecauda.moneda );
                   cRecauda.periodosolicitud  := SUBSTR(cRecauda.referencias,12,4);
                   cRecauda.numerosolicitud   := SUBSTR(SUBSTR(TRIM(cRecauda.referencias),16,20),1,LENGTH(SUBSTR(TRIM(cRecauda.referencias),16,20))-9 );
                       
                    SELECT b.CIP INTO cRecauda.codigosocio 
                      FROM PRESTAMO a,PERSONA b 
                      WHERE a.PERIODOSOLICITUD=cRecauda.periodosolicitud 
                        AND a.NUMEROSOLICITUD=cRecauda.numerosolicitud 
                        AND b.CODIGOPERSONA=a.CODIGOPERSONA 
                        AND ROWNUM = 1;
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20020,'  cRecauda.referencias ' || cRecauda.referencias);
              END;
              --
              BEGIN
                   cRecauda.importeorigen := SUBSTR(linebuf, 81, 15);
                   cRecauda.importeorigen := TO_NUMBER(SUBSTR(cRecauda.importeorigen,1,LENGTH(cRecauda.importeorigen)-2)||'.'||substr(cRecauda.importeorigen,-2),'999999.99');
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20030,'  cRecauda.importeorigen ' ||   cRecauda.importeorigen );
              END;
              --
              BEGIN
                   cRecauda.importedepositado := SUBSTR(linebuf, 96, 15 );
                   cRecauda.importedepositado := TO_NUMBER(SUBSTR(cRecauda.importedepositado,1,LENGTH(cRecauda.importedepositado)-2)||'.'||substr(cRecauda.importedepositado,-2),'999999.99');
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20040,'  cRecauda.importedepositado ' ||cRecauda.importedepositado );
              END;
              --
              BEGIN
                   cRecauda.importemora := SUBSTR(linebuf, 111,  15 );
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20050,' cRecauda.importemora ' ||cRecauda.importemora );
              END;
              --
              BEGIN
                   cRecauda.oficinapago := SUBSTR(linebuf, 126, 4 );
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20060,'  cRecauda.oficinapago ' || cRecauda.oficinapago );
              END;
              --
              BEGIN
                   cRecauda.nromovimiento := SUBSTR(linebuf,130, 6);
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20070,'  cRecauda.nromovimiento' ||cRecauda.nromovimiento );
              END;
              --
              BEGIN
                   vFechaenvio := SUBSTR(TRIM(cRecauda.referencias), -6, 6);
                   cRecauda.fechaenvio := TO_DATE(SUBSTR(vFechaenvio,5,2)||'/'||SUBSTR(vFechaenvio,3,2)||'/'||SUBSTR(vFechaenvio,1,2),'DD/MM/RR');

                   vFechapago := SUBSTR(linebuf, 136, 8);
                   cRecauda.fechapago := TO_DATE(SUBSTR(vFechapago,7,2)||'/'||SUBSTR(vFechapago,5,2)||'/'||SUBSTR(vFechapago,1,4),'DD/MM/RRRR');
                       
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20080,'cRecauda.fechapago ' ||cRecauda.fechapago );
              END;
              --
              BEGIN
                   P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud,cRecauda.numerosolicitud,cRecauda.debitoautomatico);
                   cRecauda.estado := '1';
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
              END;
              --
              BEGIN
                   cRecauda.fechaproceso := NULL;
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20130,' cRecauda.fechaproceso ' || cRecauda.fechaproceso  );
              END;
              --
              BEGIN
                   cRecauda.usuarioproceso := NULL;
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20140,' cRecauda.usuarioproceso ' || cRecauda.usuarioproceso);
              END;
              --
              IF cRecauda.tipopago = 'ACT' THEN
                 BEGIN
                      SELECT MIN(numerocuota)
                        INTO vNumerocuota
                        FROM prestamocuotas 
                       WHERE periodosolicitud = cRecauda.periodosolicitud 
                         AND numerosolicitud =  cRecauda.numerosolicitud 
                         AND estado = 2;
                 EXCEPTION WHEN OTHERS THEN
                           vNumerocuota := NULL;       
                 END; 
                 --
                 cRecauda.cuotacronograma := vNumerocuota;
                 --
                 cRecauda.amortizacion   := pkg_prestamocuotas.F_OBT_AMORTIZACION( cRecauda.numerosolicitud, 
                                                                                   cRecauda.periodosolicitud, 
                                                                                   vNumerocuota);
                 --
                 cRecauda.interes        := pkg_prestamocuotas.F_OBT_INTERES( cRecauda.numerosolicitud,
                                                                              cRecauda.periodosolicitud, 
                                                                              vNumerocuota );
                 cRecauda.mora           := 0;
                 -- 
                 cRecauda.reajuste       := pkg_prestamocuotas.F_OBT_REAJUSTE( cRecauda.numerosolicitud, 
                                                                               cRecauda.periodosolicitud, 
                                                                               vNumerocuota);
                 -- 
                 cRecauda.portes         := pkg_prestamocuotas.F_OBT_PORTES( cRecauda.numerosolicitud, 
                                                                             cRecauda.periodosolicitud, 
                                                                             vNumerocuota);
                 --
                 cRecauda.segurointeres  := pkg_prestamocuotas.F_OBT_SEGUROINTERES( cRecauda.numerosolicitud, 
                                                                                    cRecauda.periodosolicitud, 
                                                                                    vNumerocuota); 
                     
              ELSIF cRecauda.tipopago = 'ATR' THEN
                    SELECT SUM(SALDOCAPITAL)
                      INTO cRecauda.amortizacion
                      FROM TABLE( CRE08070.DEUDACUOTASSIP( cRecauda.periodosolicitud, 
                                                           cRecauda.numerosolicitud, 
                                                           cRecauda.fechaenvio ) ) 
                     WHERE FECHAVENCIMIENTO <= cRecauda.fechaenvio
                       AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                    --                                                         
                    cRecauda.interes         := pkg_recaudacionbanco.F_OBT_SALDOINTERES( cRecauda.periodosolicitud, 
                                                                                         cRecauda.numerosolicitud, 
                                                                                         cRecauda.fechaenvio );
                    --
                    cRecauda.mora            := pkg_recaudacionbanco.F_OBT_SALDOMORA( cRecauda.periodosolicitud, 
                                                                                      cRecauda.numerosolicitud, 
                                                                                      cRecauda.fechaenvio );
                    --
                    cRecauda.reajuste       := pkg_recaudacionbanco.F_OBT_SALDOREAJUSTE( cRecauda.periodosolicitud, 
                                                                                         cRecauda.numerosolicitud, 
                                                                                         cRecauda.fechaenvio );
                    -- 
                    cRecauda.portes         := pkg_recaudacionbanco.F_OBT_SALDOPORTES( cRecauda.periodosolicitud, 
                                                                                       cRecauda.numerosolicitud, 
                                                                                       cRecauda.fechaenvio );
                    --
                    cRecauda.segurointeres  := pkg_recaudacionbanco.F_OBT_SALDOSEGUROINTERES( cRecauda.periodosolicitud, 
                                                                                              cRecauda.numerosolicitud, 
                                                                                              cRecauda.fechaenvio );
              END IF;
              --
              cRecauda.totalcuota := NVL(cRecauda.amortizacion,0) +
                                     NVL(cRecauda.interes,0) +
                                     NVL(cRecauda.mora,0) +
                                     NVL(cRecauda.reajuste,0) +
                                     NVL(cRecauda.portes,0) +
                                     NVL(cRecauda.segurointeres,0);
              --
              IF cRecauda.tipopago = 'ACT' THEN
                 IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
                    cRecauda.observaciones := cRecauda.observaciones||' CUOTAS DIFERENTES '||CHR(9);
                 END IF;
                 IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                    cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
                 END IF;
              ELSE
                 IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                    cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
                 END IF;
              END IF;
              --
              BEGIN
                  INSERT INTO recaudacionbanco( fechacarga,
                                                usuariocarga,
                                                codigosocio,
                                                nombrecliente,
                                                referencias,
                                                importeorigen,
                                                importedepositado,
                                                importemora,
                                                oficinapago,
                                                nromovimiento,
                                                fechapago,
                                                tipopago,
                                                estado,
                                                codigobanco,
                                                numerocuentabanco,
                                                periodosolicitud,
                                                numerosolicitud,
                                                moneda,
                                                numerocuota,
                                                fechavencimiento,
                                                amortizacion,
                                                interes,
                                                mora,
                                                reajuste,
                                                portes,
                                                segurointeres,
                                                fechaproceso,
                                                usuarioproceso,
                                                trama,
                                                fechaenvio,
                                                debitoautomatico,
                                                cuotacronograma,
                                                totalcuota,
                                                observaciones
                                               )
                       VALUES ( cRecauda.fechacarga,
                                cRecauda.usuariocarga,
                                cRecauda.codigosocio,
                                cRecauda.nombrecliente,
                                cRecauda.referencias,
                                cRecauda.importeorigen,
                                cRecauda.importedepositado,
                                cRecauda.importemora,
                                cRecauda.oficinapago,
                                cRecauda.nromovimiento,
                                cRecauda.fechapago,
                                cRecauda.tipopago,
                                cRecauda.estado,
                                cRecauda.codigobanco,
                                cRecauda.numerocuentabanco,
                                cRecauda.periodosolicitud,
                                cRecauda.numerosolicitud,
                                cRecauda.moneda,
                                cRecauda.numerocuota,
                                cRecauda.fechavencimiento,
                                cRecauda.amortizacion,
                                cRecauda.interes,
                                cRecauda.mora,
                                cRecauda.reajuste,
                                cRecauda.portes,
                                cRecauda.segurointeres,
                                cRecauda.fechaproceso,
                                cRecauda.usuarioproceso,
                                cRecauda.trama,
                                cRecauda.fechaenvio,
                                cRecauda.debitoautomatico,
                                cRecauda.cuotacronograma,
                                cRecauda.totalcuota,
                                cRecauda.observaciones
                              ) ;
             -- commit;
                  cRecauda.observaciones := NULL;
              EXCEPTION WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20200,'Error al insertar'||sqlerrm);
              END;
              END IF;
          END IF;
      END LOOP;
      UTL_FILE.FCLOSE (in_file);
EXCEPTION WHEN NO_DATA_FOUND THEN
               UTL_FILE.FCLOSE (in_file);
          WHEN OTHERS THEN
               RAISE_APPLICATION_ERROR(-20300,'Error CargaLiquidacion: '||SQLERRM);
               UTL_FILE.FCLOSE (in_file);
END;
--
PROCEDURE P_GEN_CARGASCOTIABANK( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
                            PIFechaProceso IN DATE:=SYSDATE
                          ) IS
  in_file    UTL_FILE.FILE_TYPE;
  linebuf    VARCHAR2 (1000);
  cRecauda   recaudacionbanco%ROWTYPE;
  vFechapago VARCHAR2(8);
  vFechaenvio VARCHAR2(8);
  vAnocuota  VARCHAR2(4);
  vValidaTrama  NUMBER;
  vFechaProceso  DATE:=SYSDATE;
vNumerocuota  prestamocuotas.numerocuota%TYPE;
BEGIN
      --
      in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
      LOOP
          UTL_FILE.GET_LINE (in_file, linebuf);
          --
          linebuf := trim(linebuf);

               IF linebuf IS NOT NULL AND substr(linebuf,1,1 ) = 'D' THEN
                SELECT COUNT(*) INTO vValidaTrama 
                  FROM RECAUDACIONBANCO 
                 WHERE REPLACE(TRIM(TRAMA),' ','')= REPLACE(TRIM(linebuf),' ','');
                IF vValidaTrama = 0 THEN
                
                  cRecauda.trama := linebuf;
                  -- 02.01.2020 - David Chara Inca.
                  --cRecauda.fechacarga   := SYSDATE;
                  cRecauda.fechacarga   := vFechaProceso;
                  cRecauda.usuariocarga := USER;
                  cRecauda.codigobanco  := 5; -- Codigo Banco en Datosbanco
                  --
                  BEGIN
                       cRecauda.nombrecliente    := SUBSTR(linebuf, 49, 20 );
                       cRecauda.numerocuota      := SUBSTR(SUBSTR(linebuf, 34, 15 ),-3,3);
                       DBMS_OUTPUT.PUT_LINE('Numero cuota: '|| cRecauda.numerocuota);                       
                       -- 02.01.2020 - David Chara Inca.
                       --vAnocuota := TO_CHAR(SYSDATE,'RRRR');
                       vAnocuota := TO_CHAR(vFechaProceso,'RRRR');
                       --
                       cRecauda.fechavencimiento := TO_DATE (
                                                    SUBSTR(SUBSTR(linebuf, 139, 8 ),7,2)||'/'||
                                                    SUBSTR(SUBSTR(linebuf, 139, 8 ),5,2)||'/'||
                                                    vAnocuota,
                                                    'DD/MM/RRRR');
                       --
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20010,' cRecauda.nombrecliente ' ||cRecauda.nombrecliente);
                  END;
                  --
                  BEGIN

                       cRecauda.referencias := TRIM(SUBSTR(linebuf, 170, 30 ));
                       cRecauda.codigosocio := TRIM(SUBSTR(linebuf,19,15));
                       cRecauda.tipopago         := SUBSTR(TRIM(cRecauda.referencias),-3);
                       IF SUBSTR(linebuf,69,4) = '0000' THEN
                          cRecauda.moneda           := 1;
                       ELSE
                          cRecauda.moneda           := 2;
                       END IF;
                       cRecauda.numerocuentabanco := pkg_datosbanco.f_obt_cuentabancorecauda( cRecauda.codigobanco, cRecauda.moneda );
                       cRecauda.periodosolicitud  := SUBSTR( cRecauda.referencias,11,4);
                       cRecauda.numerosolicitud   := SUBSTR(SUBSTR(TRIM(cRecauda.referencias),15,20),1,LENGTH(SUBSTR(TRIM(cRecauda.referencias),15,20))-3 );
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20020,'  cRecauda.referencias ' || cRecauda.referencias);
                  END;
                  --
                  BEGIN
                       cRecauda.importeorigen := SUBSTR(linebuf, 73, 11);
                       cRecauda.importeorigen := TO_NUMBER(SUBSTR(cRecauda.importeorigen,1,LENGTH(cRecauda.importeorigen)-2)||'.'||substr(cRecauda.importeorigen,-2),'999999.99');
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20030,'  cRecauda.importeorigen ' ||   cRecauda.importeorigen );
                  END;
                  --
                  BEGIN
                       cRecauda.importedepositado := SUBSTR(linebuf, 73, 11);
                       cRecauda.importedepositado := TO_NUMBER(SUBSTR(cRecauda.importedepositado,1,LENGTH(cRecauda.importedepositado)-2)||'.'||substr(cRecauda.importedepositado,-2),'999999.99');
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20040,'  cRecauda.importedepositado ' ||cRecauda.importedepositado );
                  END;
                  --
                  BEGIN
                       cRecauda.importemora := SUBSTR(linebuf, 128,  11 );
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20050,' cRecauda.importemora ' ||cRecauda.importemora );
                  END;
                  --
                  BEGIN
                       cRecauda.oficinapago := 0;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20060,'  cRecauda.oficinapago ' || cRecauda.oficinapago );
                  END;
                  --
                  BEGIN
                       cRecauda.nromovimiento := SUBSTR(linebuf,157, 13);
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20070,'  cRecauda.nromovimiento' ||cRecauda.nromovimiento );
                  END;
                  --
                  BEGIN
                       vFechaenvio := SUBSTR(linebuf, 170, 6);
                       DBMS_OUTPUT.PUT_LINE('Fecha envio: '|| vFechaenvio);
                       cRecauda.fechaenvio := TO_DATE(SUBSTR(vFechaenvio,5,2)||'/'||SUBSTR(vFechaenvio,3,2)||'/'||SUBSTR(vFechaenvio,1,2),'DD/MM/YY');
                       
                       vFechapago := SUBSTR(linebuf, 208, 8);
                       DBMS_OUTPUT.PUT_LINE('Fecha pago: '|| vFechapago);
                       cRecauda.fechapago := TO_DATE(SUBSTR(vFechapago,7,2)||'/'||SUBSTR(vFechapago,5,2)||'/'||SUBSTR(vFechapago,1,4),'DD/MM/YYYY');
                       
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20080,'cRecauda.fechapago ' ||cRecauda.fechapago );
                  END;
                  --
                  BEGIN                  
                       cRecauda.estado := '1';
                       P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud,cRecauda.numerosolicitud,cRecauda.debitoautomatico);
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
                  END;
                  --
                  BEGIN
                       cRecauda.fechaproceso := NULL;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20130,' cRecauda.fechaproceso ' || cRecauda.fechaproceso  );
                  END;
                  --
                  BEGIN
                       cRecauda.usuarioproceso := NULL;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20140,' cRecauda.usuarioproceso ' || cRecauda.usuarioproceso  );
                  END;
                  --
                  IF cRecauda.tipopago = 'ACT' THEN
                     BEGIN
                          SELECT MIN(numerocuota)
                            INTO vNumerocuota
                            FROM prestamocuotas 
                           WHERE periodosolicitud = cRecauda.periodosolicitud 
                             AND numerosolicitud =  cRecauda.numerosolicitud 
                             AND estado = 2;
                     EXCEPTION WHEN OTHERS THEN
                               vNumerocuota := NULL;       
                     END; 
                     --
                     cRecauda.cuotacronograma := vNumerocuota;
                     --
                     cRecauda.amortizacion   := pkg_prestamocuotas.F_OBT_AMORTIZACION( cRecauda.numerosolicitud, 
                                                                                       cRecauda.periodosolicitud, 
                                                                                       vNumerocuota);
                     --
                     cRecauda.interes        := pkg_prestamocuotas.F_OBT_INTERES( cRecauda.numerosolicitud,
                                                                                  cRecauda.periodosolicitud, 
                                                                                  vNumerocuota );
                     cRecauda.mora           := 0;
                     -- 
                     cRecauda.reajuste       := pkg_prestamocuotas.F_OBT_REAJUSTE( cRecauda.numerosolicitud, 
                                                                                   cRecauda.periodosolicitud, 
                                                                                   vNumerocuota);
                     -- 
                     cRecauda.portes         := pkg_prestamocuotas.F_OBT_PORTES( cRecauda.numerosolicitud, 
                                                                                 cRecauda.periodosolicitud, 
                                                                                 vNumerocuota);
                     --
                     cRecauda.segurointeres  := pkg_prestamocuotas.F_OBT_SEGUROINTERES( cRecauda.numerosolicitud, 
                                                                                        cRecauda.periodosolicitud, 
                                                                                        vNumerocuota); 
                         
                  ELSIF cRecauda.tipopago = 'ATR' THEN
                        SELECT SUM(SALDOCAPITAL)
                          INTO cRecauda.amortizacion
                          FROM TABLE( CRE08070.DEUDACUOTASSIP( cRecauda.periodosolicitud, 
                                                               cRecauda.numerosolicitud, 
                                                               cRecauda.fechaenvio ) ) 
                         WHERE FECHAVENCIMIENTO <= cRecauda.fechaenvio
                           AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                        --                                                         
                        cRecauda.interes         := pkg_recaudacionbanco.F_OBT_SALDOINTERES( cRecauda.periodosolicitud, 
                                                                                             cRecauda.numerosolicitud, 
                                                                                             cRecauda.fechaenvio );
                        --
                        cRecauda.mora            := pkg_recaudacionbanco.F_OBT_SALDOMORA( cRecauda.periodosolicitud, 
                                                                                          cRecauda.numerosolicitud, 
                                                                                          cRecauda.fechaenvio );
                        --
                        cRecauda.reajuste       := pkg_recaudacionbanco.F_OBT_SALDOREAJUSTE( cRecauda.periodosolicitud, 
                                                                                             cRecauda.numerosolicitud, 
                                                                                             cRecauda.fechaenvio );
                        -- 
                        cRecauda.portes         := pkg_recaudacionbanco.F_OBT_SALDOPORTES( cRecauda.periodosolicitud, 
                                                                                           cRecauda.numerosolicitud, 
                                                                                           cRecauda.fechaenvio );
                        --
                        cRecauda.segurointeres  := pkg_recaudacionbanco.F_OBT_SALDOSEGUROINTERES( cRecauda.periodosolicitud, 
                                                                                                  cRecauda.numerosolicitud, 
                                                                                                  cRecauda.fechaenvio );
                  END IF;
                  --
                  cRecauda.totalcuota := NVL(cRecauda.amortizacion,0) +
                                         NVL(cRecauda.interes,0) +
                                         NVL(cRecauda.mora,0) +
                                         NVL(cRecauda.reajuste,0) +
                                         NVL(cRecauda.portes,0) +
                                         NVL(cRecauda.segurointeres,0);
                  --
                  IF cRecauda.tipopago = 'ACT' THEN
                     IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
                        cRecauda.observaciones := cRecauda.observaciones||' CUOTAS DIFERENTES '||CHR(9);
                     END IF;
                     IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                        cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
                     END IF;
                  ELSE
                     IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                        cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
                     END IF;
                  END IF;
                  --
                  BEGIN
                      INSERT INTO recaudacionbanco( fechacarga,
                                                    usuariocarga,
                                                    codigosocio,
                                                    nombrecliente,
                                                    referencias,
                                                    importeorigen,
                                                    importedepositado,
                                                    importemora,
                                                    oficinapago,
                                                    nromovimiento,
                                                    fechapago,
                                                    tipopago,
                                                    estado,
                                                    codigobanco,
                                                    numerocuentabanco,
                                                    periodosolicitud,
                                                    numerosolicitud,
                                                    moneda,
                                                    numerocuota,
                                                    fechavencimiento,
                                                    amortizacion,
                                                    interes,
                                                    mora,
                                                    reajuste,
                                                    portes,
                                                    segurointeres,
                                                    fechaproceso,
                                                    usuarioproceso,
                                                    trama,
                                                    fechaenvio,
                                                    debitoautomatico,
                                                    cuotacronograma,
                                                    totalcuota,
                                                    observaciones
                                                   )
                           VALUES ( cRecauda.fechacarga,
                                    cRecauda.usuariocarga,
                                    cRecauda.codigosocio,
                                    cRecauda.nombrecliente,
                                    cRecauda.referencias,
                                    cRecauda.importeorigen,
                                    cRecauda.importedepositado,
                                    cRecauda.importemora,
                                    cRecauda.oficinapago,
                                    cRecauda.nromovimiento,
                                    cRecauda.fechapago,
                                    cRecauda.tipopago,
                                    cRecauda.estado,
                                    cRecauda.codigobanco,
                                    cRecauda.numerocuentabanco,
                                    cRecauda.periodosolicitud,
                                    cRecauda.numerosolicitud,
                                    cRecauda.moneda,
                                    cRecauda.numerocuota,
                                    cRecauda.fechavencimiento,
                                    cRecauda.amortizacion,
                                    cRecauda.interes,
                                    cRecauda.mora,
                                    cRecauda.reajuste,
                                    cRecauda.portes,
                                    cRecauda.segurointeres,
                                    cRecauda.fechaproceso,
                                    cRecauda.usuarioproceso,
                                    cRecauda.trama,
                                    cRecauda.fechaenvio,
                                    cRecauda.debitoautomatico,
                                    cRecauda.cuotacronograma,
                                    cRecauda.totalcuota,
                                    cRecauda.observaciones
                                  ) ;
                      cRecauda.observaciones := NULL;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20200,'Error al insertar'||sqlerrm);
                  END;
                  END IF;
               END IF;
      END LOOP;
      UTL_FILE.FCLOSE (in_file);
EXCEPTION WHEN NO_DATA_FOUND THEN
               UTL_FILE.FCLOSE (in_file);
          WHEN OTHERS THEN
               RAISE_APPLICATION_ERROR(-20300,'Error CargaLiquidacion: '||SQLERRM);
               UTL_FILE.FCLOSE (in_file);
END P_GEN_CARGASCOTIABANK;
--
PROCEDURE P_GEN_CARGACREDITO( PIDirectorio    VARCHAR2,
                              PINombreArchivo VARCHAR2,
                              PIFechaProceso IN DATE:=SYSDATE
                            ) IS
  in_file    UTL_FILE.FILE_TYPE;
  linebuf    VARCHAR2 (1000);
  cRecauda   recaudacionbanco%ROWTYPE;
  vFechapago VARCHAR2(8);
  vFechaEnvio   VARCHAR2(8);
  vAnocuota  VARCHAR2(4);
  vValidaTrama  NUMBER;
  vFechaProceso  DATE:=SYSDATE;
vNumerocuota  prestamocuotas.numerocuota%TYPE;
BEGIN
      --
      DBMS_OUTPUT.PUT_LINE('0');
      in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
      DBMS_OUTPUT.PUT_LINE('1');
      --
      LOOP
          UTL_FILE.GET_LINE (in_file, linebuf);
          --
          linebuf := trim(linebuf);

               IF linebuf IS NOT NULL AND substr(linebuf,1,2 ) = 'DD' THEN
                SELECT COUNT(*) INTO vValidaTrama 
                  FROM RECAUDACIONBANCO
                 WHERE REPLACE(TRIM(TRAMA),' ','')= REPLACE(TRIM(linebuf),' ','');
                DBMS_OUTPUT.PUT_LINE('vValidaTrama: ' || vValidaTrama);
                IF vValidaTrama=0 THEN
                  cRecauda.trama := linebuf;
                  -- 02.01.2020 - David Chara Inca.
                  --cRecauda.fechacarga   := SYSDATE;
                  cRecauda.fechacarga   := vFechaProceso;
                  cRecauda.usuariocarga := USER;
                  cRecauda.codigobanco  := 3; -- Codigo Banco en Datosbanco
                  --
                  BEGIN
                       cRecauda.codigosocio      := TRIM(SUBSTR(SUBSTR(linebuf, 14, 14 ),-7));
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20010,' cRecauda.codigosocio ' ||cRecauda.codigosocio);
                  END;
                  --
                  BEGIN
                       cRecauda.nombrecliente    := TRIM(SUBSTR(linebuf, 28, 30 ));
DBMS_OUTPUT.PUT_LINE(SUBSTR(TRIM(cRecauda.nombrecliente),25,3));
                       cRecauda.numerocuota      := SUBSTR(TRIM(cRecauda.nombrecliente),28,3);--SUBSTR(SUBSTR(TRIM(cRecauda.nombrecliente),-7),1,3);
                       -- 02.01.2020 - David Chara Inca.
                       --vAnocuota := TO_CHAR(SYSDATE,'RRRR');
                       vAnocuota := TO_CHAR(vFechaProceso,'RRRR');
                       cRecauda.tipopago         := SUBSTR(TRIM(cRecauda.nombrecliente),25,3);
                       IF SUBSTR(TRIM(cRecauda.nombrecliente),13,1) = 'S' THEN
                          cRecauda.moneda           := 1;
                       ELSIF SUBSTR(TRIM(cRecauda.nombrecliente),13,1) = 'D' THEN
                          cRecauda.moneda           := 2;
                       ELSE
                          RAISE_APPLICATION_ERROR(-20071,' Moneda No Existe' ||cRecauda.nromovimiento );  
                       END IF;
                       --
DBMS_OUTPUT.PUT_LINE(' cRecauda.nombrecliente '||cRecauda.nombrecliente);
                       cRecauda.numerocuentabanco := pkg_datosbanco.f_obt_cuentabancorecauda( cRecauda.codigobanco, cRecauda.moneda );
                       cRecauda.periodosolicitud  := SUBSTR(cRecauda.nombrecliente,14,4);
                       cRecauda.numerosolicitud   := SUBSTR(cRecauda.nombrecliente,18,7); --SUBSTR(SUBSTR(TRIM(cRecauda.nombrecliente),21,20),1,LENGTH(SUBSTR(TRIM(cRecauda.nombrecliente),21,20))-3 );
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20010,' cRecauda.nombrecliente ' ||cRecauda.nombrecliente||sqlerrm);
                  END;
                  --
                  BEGIN
                       --vFechapago := SUBSTR(linebuf, 58, 8);
                       vFechapago := SUBSTR(linebuf, 58, 8);                      
                         cRecauda.fechapago := TO_DATE(SUBSTR(vFechapago,7,2)||'/'||
                         SUBSTR(vFechapago,5,2)||'/'||
                         SUBSTR(vFechapago,1,4),'DD/MM/YYYY');                       
                         
                      vFechaEnvio := SUBSTR(linebuf, 31, 6);                      
                         cRecauda.fechaenvio := TO_DATE(SUBSTR(vFechaEnvio,5,2)||'/'||
                         SUBSTR(vFechaEnvio,3,2)||'/'||
                         SUBSTR(vFechaEnvio,1,2),'DD/MM/YY');
                         
                         
                                                    
                        cRecauda.fechavencimiento := TO_DATE (
                        SUBSTR(SUBSTR(linebuf, 66, 30 ),7,2)||'/'||
                        SUBSTR(SUBSTR(linebuf, 66, 30 ),5,2)||'/'||
                        SUBSTR(SUBSTR(linebuf, 66, 30 ),1,4),
                        'DD/MM/RRRR');

                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20080,'cRecauda.fechapago ' ||cRecauda.fechapago );
                  END;
                  --
                  BEGIN
                       cRecauda.referencias      := SUBSTR(linebuf, 131, 25 );
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20020,'  cRecauda.referencias ' || cRecauda.referencias);
                  END;
                  --
                  BEGIN
                       cRecauda.importeorigen := SUBSTR(linebuf, 74, 15);
                       cRecauda.importeorigen := TO_NUMBER(SUBSTR(cRecauda.importeorigen,1,LENGTH(cRecauda.importeorigen)-2)||'.'||substr(cRecauda.importeorigen,-2),'999999.99');
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20030,'  cRecauda.importeorigen ' ||   cRecauda.importeorigen );
                  END;
                  --
                  BEGIN
                       cRecauda.importemora := SUBSTR(linebuf, 89,  15 );
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20050,' cRecauda.importemora ' ||cRecauda.importemora );
                  END;
                  --
                  BEGIN
                       cRecauda.importedepositado := SUBSTR(linebuf, 104, 15 );
                       cRecauda.importedepositado := TO_NUMBER(SUBSTR(cRecauda.importedepositado,1,LENGTH(cRecauda.importedepositado)-2)||'.'||substr(cRecauda.importedepositado,-2),'999999.99');
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20040,'  cRecauda.importedepositado ' ||cRecauda.importedepositado );
                  END;
                  --
                  BEGIN
                       cRecauda.nromovimiento := SUBSTR(linebuf,125, 6);
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20070,'  cRecauda.nromovimiento' ||cRecauda.nromovimiento );
                  END;
                  --
                  BEGIN
                       cRecauda.estado := '1';
                       P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud,cRecauda.numerosolicitud,cRecauda.debitoautomatico);
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
                  END;
                  --
                  BEGIN
                       cRecauda.fechaproceso := NULL;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20130,' cRecauda.fechaproceso ' || cRecauda.fechaproceso  );
                  END;
                  --
                  BEGIN
                       cRecauda.usuarioproceso := NULL;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20140,' cRecauda.usuarioproceso ' || cRecauda.usuarioproceso  );
                  END;
                  --
                  IF cRecauda.tipopago = 'ACT' THEN
                     BEGIN
                          SELECT MIN(numerocuota)
                            INTO vNumerocuota
                            FROM prestamocuotas 
                           WHERE periodosolicitud = cRecauda.periodosolicitud 
                             AND numerosolicitud =  cRecauda.numerosolicitud 
                             AND estado = 2;
                     EXCEPTION WHEN OTHERS THEN
                               vNumerocuota := NULL;       
                     END; 
                     --
                     cRecauda.cuotacronograma := vNumerocuota;
                     --
                     cRecauda.amortizacion   := pkg_prestamocuotas.F_OBT_AMORTIZACION( cRecauda.numerosolicitud, 
                                                                                       cRecauda.periodosolicitud, 
                                                                                       vNumerocuota);
                     --
                     cRecauda.interes        := pkg_prestamocuotas.F_OBT_INTERES( cRecauda.numerosolicitud,
                                                                                  cRecauda.periodosolicitud, 
                                                                                  vNumerocuota );
                     cRecauda.mora           := 0;
                     -- 
                     cRecauda.reajuste       := pkg_prestamocuotas.F_OBT_REAJUSTE( cRecauda.numerosolicitud, 
                                                                                   cRecauda.periodosolicitud, 
                                                                                   vNumerocuota);
                     -- 
                     cRecauda.portes         := pkg_prestamocuotas.F_OBT_PORTES( cRecauda.numerosolicitud, 
                                                                                 cRecauda.periodosolicitud, 
                                                                                 vNumerocuota);
                     --
                     cRecauda.segurointeres  := pkg_prestamocuotas.F_OBT_SEGUROINTERES( cRecauda.numerosolicitud, 
                                                                                        cRecauda.periodosolicitud, 
                                                                                        vNumerocuota); 
                         
                  ELSIF cRecauda.tipopago = 'ATR' THEN
                        SELECT SUM(SALDOCAPITAL)
                          INTO cRecauda.amortizacion
                          FROM TABLE( CRE08070.DEUDACUOTASSIP( cRecauda.periodosolicitud, 
                                                               cRecauda.numerosolicitud, 
                                                               cRecauda.fechaenvio ) ) 
                         WHERE FECHAVENCIMIENTO <= cRecauda.fechaenvio
                           AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                        --                                                         
                        cRecauda.interes         := pkg_recaudacionbanco.F_OBT_SALDOINTERES( cRecauda.periodosolicitud, 
                                                                                             cRecauda.numerosolicitud, 
                                                                                             cRecauda.fechaenvio );
                        --
                        cRecauda.mora            := pkg_recaudacionbanco.F_OBT_SALDOMORA( cRecauda.periodosolicitud, 
                                                                                          cRecauda.numerosolicitud, 
                                                                                          cRecauda.fechaenvio );
                        --
                        cRecauda.reajuste       := pkg_recaudacionbanco.F_OBT_SALDOREAJUSTE( cRecauda.periodosolicitud, 
                                                                                             cRecauda.numerosolicitud, 
                                                                                             cRecauda.fechaenvio );
                        -- 
                        cRecauda.portes         := pkg_recaudacionbanco.F_OBT_SALDOPORTES( cRecauda.periodosolicitud, 
                                                                                           cRecauda.numerosolicitud, 
                                                                                           cRecauda.fechaenvio );
                        --
                        cRecauda.segurointeres  := pkg_recaudacionbanco.F_OBT_SALDOSEGUROINTERES( cRecauda.periodosolicitud, 
                                                                                                  cRecauda.numerosolicitud, 
                                                                                                  cRecauda.fechaenvio );
                  END IF;
                  --
                  cRecauda.totalcuota := NVL(cRecauda.amortizacion,0) +
                                         NVL(cRecauda.interes,0) +
                                         NVL(cRecauda.mora,0) +
                                         NVL(cRecauda.reajuste,0) +
                                         NVL(cRecauda.portes,0) +
                                         NVL(cRecauda.segurointeres,0);

                  --
                  IF cRecauda.tipopago = 'ACT' THEN
                     IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
                        cRecauda.observaciones := cRecauda.observaciones||' CUOTAS DIFERENTES '||CHR(9);
                     END IF;
                     IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                        cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
                     END IF;
                  ELSE
                     IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                        cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
                     END IF;
                  END IF;
                  --
                  BEGIN
                      INSERT INTO recaudacionbanco( fechacarga,
                                                    usuariocarga,
                                                    codigosocio,
                                                    nombrecliente,
                                                    referencias,
                                                    importeorigen,
                                                    importedepositado,
                                                    importemora,
                                                    oficinapago,
                                                    nromovimiento,
                                                    fechapago,
                                                    tipopago,
                                                    estado,
                                                    codigobanco,
                                                    numerocuentabanco,
                                                    periodosolicitud,
                                                    numerosolicitud,
                                                    moneda,
                                                    numerocuota,
                                                    fechavencimiento,
                                                    amortizacion,
                                                    interes,
                                                    mora,
                                                    reajuste,
                                                    portes,
                                                    segurointeres,
                                                    fechaproceso,
                                                    usuarioproceso,
                                                    trama,
                                                    fechaenvio,
                                                    debitoautomatico,
                                                    cuotacronograma,
                                                    totalcuota,
                                                    observaciones
                                                   )
                           VALUES ( cRecauda.fechacarga,
                                    cRecauda.usuariocarga,
                                    cRecauda.codigosocio,
                                    cRecauda.nombrecliente,
                                    cRecauda.referencias,
                                    cRecauda.importeorigen,
                                    cRecauda.importedepositado,
                                    cRecauda.importemora,
                                    cRecauda.oficinapago,
                                    cRecauda.nromovimiento,
                                    cRecauda.fechapago,
                                    cRecauda.tipopago,
                                    cRecauda.estado,
                                    cRecauda.codigobanco,
                                    cRecauda.numerocuentabanco,
                                    cRecauda.periodosolicitud,
                                    cRecauda.numerosolicitud,
                                    cRecauda.moneda,
                                    cRecauda.numerocuota,
                                    cRecauda.fechavencimiento,
                                    cRecauda.amortizacion,
                                    cRecauda.interes,
                                    cRecauda.mora,
                                    cRecauda.reajuste,
                                    cRecauda.portes,
                                    cRecauda.segurointeres,
                                    cRecauda.fechaproceso,
                                    cRecauda.usuarioproceso,
                                    cRecauda.trama,
                                    cRecauda.fechaenvio,
                                    cRecauda.debitoautomatico,
                                    cRecauda.cuotacronograma,
                                    cRecauda.totalcuota,
                                    cRecauda.observaciones
                                  ) ;
                        cRecauda.observaciones := NULL;
                  EXCEPTION WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20200,'Error al insertar'||sqlerrm);
                  END;
                END IF;
               END IF;
      END LOOP;
      UTL_FILE.FCLOSE (in_file);
EXCEPTION WHEN NO_DATA_FOUND THEN
               UTL_FILE.FCLOSE (in_file);
          WHEN OTHERS THEN
               RAISE_APPLICATION_ERROR(-20300,'Error CargaLiquidacion: '||SQLERRM);
               UTL_FILE.FCLOSE (in_file);
END P_GEN_CARGACREDITO;
--
PROCEDURE P_GEN_CARGAPICHINCHA( PIDirectorio    VARCHAR2,
                            PINombreArchivo VARCHAR2,
                            PIFechaProceso IN DATE:=SYSDATE
                          ) IS
  --
  in_file UTL_FILE.FILE_TYPE;
  cRecauda   recaudacionbanco%ROWTYPE;
  --
  linebuf   VARCHAR2 (1000);
  --
  xtam number;
  xpos number;
  --
  vSeparador    VARCHAR2(1):= CHR(09);
  vV01 VARCHAR2(7);
  vV02 VARCHAR2(3);
  vV03 VARCHAR2(8);
  vV04 VARCHAR2(25);
  vV05 VARCHAR2(7);
  vV06 VARCHAR2(7);
  vV07 VARCHAR2(3);
  vV08 VARCHAR2(10);
  vV09 VARCHAR2(34);
  vV10 VARCHAR2(5);
  vV11 VARCHAR2(25);
  vV12 VARCHAR2(9);
  vAnocuota  VARCHAR2(4);
  vValidaTrama  NUMBER;
  vFechaProceso  DATE:=SYSDATE;
vNumerocuota  prestamocuotas.numerocuota%TYPE;
BEGIN
      --
      in_file := UTL_FILE.FOPEN( PIDirectorio, PINombreArchivo, 'r');
      --
      LOOP
          --
          UTL_FILE.GET_LINE (in_file, linebuf);
          --
          
          linebuf := trim(linebuf);

          SELECT COUNT(*) INTO vValidaTrama 
            FROM RECAUDACIONBANCO 
           WHERE REPLACE(TRIM(TRAMA),' ','')= REPLACE(TRIM(linebuf),' ','');
          
          IF vValidaTrama=0 THEN
          cRecauda.trama        := linebuf;          
          -- 02.01.2020 - David Chara Inca.
          --cRecauda.fechacarga   := SYSDATE;
          cRecauda.fechacarga   := vFechaProceso;
          cRecauda.usuariocarga := USER;
          cRecauda.codigobanco  := 9;  -- Codigobanco en Datos Banco
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV01 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
          cRecauda.nromovimiento := vV01;
dbms_output.put_line('vV01 '||vV01);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV02 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
dbms_output.put_line('vV02 '||vV02);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV03 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
dbms_output.put_line('vV03 '||vV03);

          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV04 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
          cRecauda.referencias := vV04;
dbms_output.put_line('vReferencia '||cRecauda.referencias);
dbms_output.put_line('vFecha '||SUBSTR(TRIM(cRecauda.referencias),0,6));
          --cRecauda.codigosocio := SUBSTR(cRecauda.referencias,1,7);
          cRecauda.tipopago    := SUBSTR(TRIM(cRecauda.referencias),-3);
          cRecauda.fechaenvio := TO_DATE(SUBSTR(TRIM(cRecauda.referencias),0,6),'YYMMDD');
dbms_output.put_line('vV04 '||vV04);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
dbms_output.put_line('linebuf '||linebuf);
dbms_output.put_line('linebuf xxx '||substr(linebuf,1,xpos-1));
          vV05 := substr(linebuf,1,xpos-1);
dbms_output.put_line('vV04.1 '||vV05);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
          cRecauda.importeorigen := TO_NUMBER(SUBSTR(vV05,1,LENGTH(vV05)-2)||'.'||substr(vV05,-2),'999999.99');
          cRecauda.importedepositado := TO_NUMBER(SUBSTR(vV05,1,LENGTH(vV05)-2)||'.'||substr(vV05,-2),'999999.99');
dbms_output.put_line('vV05 '||vV05);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
dbms_output.put_line('vV05.1 '||substr(linebuf,1,xpos-1));
          vV06 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
dbms_output.put_line('vV06 '||vV06);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV07 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
          
          IF SUBSTR(TRIM(vV07),0,3) = 'PEN' THEN
            cRecauda.moneda           := 1;
          ELSE
            cRecauda.moneda           := 2;
          END IF;                    
          
dbms_output.put_line('vV07 '||vV07);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV08 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);          
          cRecauda.fechapago := TO_DATE(vV08,'DD/MM/YYYY');
--          cRecauda.fechavencimiento := TO_DATE(vV08,'DD/MM/YYYY');
dbms_output.put_line('vV08 '||vV08);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV09 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
          cRecauda.nombrecliente := TRIM(REPLACE(vV09,'"',''));
          dbms_output.put_line('1y :' || cRecauda.nombrecliente);
          cRecauda.numerocuota      := SUBSTR(SUBSTR(TRIM(cRecauda.nombrecliente),-7),1,3);
          --
          vAnocuota := TO_CHAR(vFechaProceso,'RRRR');
          dbms_output.put_line('2y');
          cRecauda.fechavencimiento := TO_DATE (
                                       SUBSTR(SUBSTR(TRIM(cRecauda.nombrecliente),-4),3,2)||'/'||
                                       SUBSTR(SUBSTR(TRIM(cRecauda.nombrecliente),-4),1,2)||'/'||
                                       vAnocuota,
                                       'DD/MM/RRRR');

dbms_output.put_line('vV09 '||vV09);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV10 := substr(linebuf,1,xpos-1);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);
dbms_output.put_line('vV10 '||vV10);
          --
          
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV11 := substr(linebuf,1,xpos-1);
          dbms_output.put_line('vV11 referencia: '||vV11);
          linebuf:=substr(linebuf,xpos+1,xtam-xpos);          
          cRecauda.periodosolicitud := substr(vV11,11,4);
          dbms_output.put_line('vV11 Numero solicitud: '||substr(vV11,15,LENGTH(SUBSTR(TRIM(vV11),14,21))-4));
          cRecauda.numerosolicitud  := substr(vV11,15,LENGTH(SUBSTR(TRIM(vV11),14,21))-4);
            dbms_output.put_line('Periodo: '||cRecauda.periodosolicitud || ' numero: '||cRecauda.numerosolicitud);
          
          SELECT b.CIP INTO cRecauda.codigosocio 
            FROM PRESTAMO a,PERSONA b 
          WHERE a.PERIODOSOLICITUD=cRecauda.periodosolicitud AND a.NUMEROSOLICITUD=cRecauda.numerosolicitud AND 
          b.CODIGOPERSONA=a.CODIGOPERSONA AND
          ROWNUM = 1;
          --
          cRecauda.numerocuentabanco := pkg_datosbanco.f_obt_cuentabancorecauda( cRecauda.codigobanco, cRecauda.moneda );
dbms_output.put_line('vV11 '||vV11);
          --
          xtam := length(linebuf);
          xpos := instr(linebuf,vSeparador);
          vV12 := trim(linebuf);
          --
          BEGIN
               cRecauda.estado := '1';
               P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud,cRecauda.numerosolicitud,cRecauda.debitoautomatico);
          EXCEPTION WHEN OTHERS THEN
                    RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
          END;
          --
          IF cRecauda.tipopago = 'ACT' THEN
             BEGIN
                  SELECT MIN(numerocuota)
                    INTO vNumerocuota
                    FROM prestamocuotas 
                   WHERE periodosolicitud = cRecauda.periodosolicitud 
                     AND numerosolicitud =  cRecauda.numerosolicitud 
                     AND estado = 2;
             EXCEPTION WHEN OTHERS THEN
                       vNumerocuota := NULL;       
             END; 
             --
             cRecauda.cuotacronograma := vNumerocuota;
             --
             cRecauda.amortizacion   := pkg_prestamocuotas.F_OBT_AMORTIZACION( cRecauda.numerosolicitud, 
                                                                               cRecauda.periodosolicitud, 
                                                                               vNumerocuota);
             --
             cRecauda.interes        := pkg_prestamocuotas.F_OBT_INTERES( cRecauda.numerosolicitud,
                                                                          cRecauda.periodosolicitud, 
                                                                          vNumerocuota );
             cRecauda.mora           := 0;
             -- 
             cRecauda.reajuste       := pkg_prestamocuotas.F_OBT_REAJUSTE( cRecauda.numerosolicitud, 
                                                                           cRecauda.periodosolicitud, 
                                                                           vNumerocuota);
             -- 
             cRecauda.portes         := pkg_prestamocuotas.F_OBT_PORTES( cRecauda.numerosolicitud, 
                                                                         cRecauda.periodosolicitud, 
                                                                         vNumerocuota);
             --
             cRecauda.segurointeres  := pkg_prestamocuotas.F_OBT_SEGUROINTERES( cRecauda.numerosolicitud, 
                                                                                cRecauda.periodosolicitud, 
                                                                                vNumerocuota); 
                         
          ELSIF cRecauda.tipopago = 'ATR' THEN
                SELECT SUM(SALDOCAPITAL)
                  INTO cRecauda.amortizacion
                  FROM TABLE( CRE08070.DEUDACUOTASSIP( cRecauda.periodosolicitud, 
                                                       cRecauda.numerosolicitud, 
                                                       cRecauda.fechaenvio ) ) 
                 WHERE FECHAVENCIMIENTO <= cRecauda.fechaenvio
                   AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
                --                                                         
                cRecauda.interes         := pkg_recaudacionbanco.F_OBT_SALDOINTERES( cRecauda.periodosolicitud, 
                                                                                     cRecauda.numerosolicitud, 
                                                                                     cRecauda.fechaenvio );
                --
                cRecauda.mora            := pkg_recaudacionbanco.F_OBT_SALDOMORA( cRecauda.periodosolicitud, 
                                                                                  cRecauda.numerosolicitud, 
                                                                                  cRecauda.fechaenvio );
                --
                cRecauda.reajuste       := pkg_recaudacionbanco.F_OBT_SALDOREAJUSTE( cRecauda.periodosolicitud, 
                                                                                     cRecauda.numerosolicitud, 
                                                                                     cRecauda.fechaenvio );
                -- 
                cRecauda.portes         := pkg_recaudacionbanco.F_OBT_SALDOPORTES( cRecauda.periodosolicitud, 
                                                                                   cRecauda.numerosolicitud, 
                                                                                   cRecauda.fechaenvio );
                --
                cRecauda.segurointeres  := pkg_recaudacionbanco.F_OBT_SALDOSEGUROINTERES( cRecauda.periodosolicitud, 
                                                                                          cRecauda.numerosolicitud, 
                                                                                          cRecauda.fechaenvio );
          END IF;
          --
          cRecauda.totalcuota := NVL(cRecauda.amortizacion,0) +
                                 NVL(cRecauda.interes,0) +
                                 NVL(cRecauda.mora,0) +
                                 NVL(cRecauda.reajuste,0) +
                                 NVL(cRecauda.portes,0) +
                                 NVL(cRecauda.segurointeres,0);
          --
          IF cRecauda.tipopago = 'ACT' THEN
             IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
                cRecauda.observaciones := cRecauda.observaciones||' CUOTAS DIFERENTES '||CHR(9);
             END IF;
             IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
             END IF;
          ELSE
             IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
                cRecauda.observaciones := cRecauda.observaciones||' IMPORTES DIFERENTES '||CHR(9);
             END IF;
          END IF;
          --
             BEGIN
                      INSERT INTO recaudacionbanco( fechacarga,
                                                    usuariocarga,
                                                    codigosocio,
                                                    nombrecliente,
                                                    referencias,
                                                    importeorigen,
                                                    importedepositado,
                                                    importemora,
                                                    oficinapago,
                                                    nromovimiento,
                                                    fechapago,
                                                    tipopago,
                                                    estado,
                                                    codigobanco,
                                                    numerocuentabanco,
                                                    periodosolicitud,
                                                    numerosolicitud,
                                                    moneda,
                                                    numerocuota,
                                                    fechavencimiento,
                                                    amortizacion,
                                                    interes,
                                                    mora,
                                                    reajuste,
                                                    portes,
                                                    segurointeres,
                                                    fechaproceso,
                                                    usuarioproceso,
                                                    trama,
                                                    fechaenvio,
                                                    debitoautomatico,
                                                    cuotacronograma,
                                                    totalcuota,
                                                    observaciones
                                                   )
                           VALUES ( cRecauda.fechacarga,
                                    cRecauda.usuariocarga,
                                    cRecauda.codigosocio,
                                    cRecauda.nombrecliente,
                                    cRecauda.referencias,
                                    cRecauda.importeorigen,
                                    cRecauda.importedepositado,
                                    cRecauda.importemora,
                                    cRecauda.oficinapago,
                                    cRecauda.nromovimiento,
                                    cRecauda.fechapago,
                                    cRecauda.tipopago,
                                    cRecauda.estado,
                                    cRecauda.codigobanco,
                                    cRecauda.numerocuentabanco,
                                    cRecauda.periodosolicitud,
                                    cRecauda.numerosolicitud,
                                    cRecauda.moneda,
                                    cRecauda.numerocuota,
                                    cRecauda.fechavencimiento,
                                    cRecauda.amortizacion,
                                    cRecauda.interes,
                                    cRecauda.mora,
                                    cRecauda.reajuste,
                                    cRecauda.portes,
                                    cRecauda.segurointeres,
                                    cRecauda.fechaproceso,
                                    cRecauda.usuarioproceso,
                                    cRecauda.trama,
                                    cRecauda.fechaenvio,
                                    cRecauda.debitoautomatico,
                                    cRecauda.cuotacronograma,
                                    cRecauda.totalcuota,
                                    cRecauda.observaciones
                                  ) ;
                  cRecauda.observaciones := NULL;
             EXCEPTION WHEN OTHERS THEN
                       RAISE_APPLICATION_ERROR(-20200,'Error al insertar'||sqlerrm);
             END ;
             END IF;
      END LOOP;
EXCEPTION WHEN NO_DATA_FOUND THEN
               UTL_FILE.FCLOSE (in_file);
          WHEN OTHERS THEN
               dbms_output.put_line('others '||sqlerrm);
               UTL_FILE.FCLOSE (in_file);
END P_GEN_CARGAPICHINCHA;
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
                              --PIFechaPago   IN DATE,
                              PIFechaBoleta   IN DATE,
                              PIFechaEnvio   IN DATE
                            ) IS

vNumerocuentaBolsa     cuentacorriente.numerocuenta%TYPE;
vComentario            caja.glosa%TYPE;
vCodigoPersonaSocio    caja.codigopersona%TYPE;
vNumerocuentaSocio     cuentacorriente.numerocuenta%TYPE;
vCodigoBanco           datosbanco.codigobanco%TYPE;
veNotieneCtaAHV        EXCEPTION;

vCodigoAgenciaCaja  caja.codigoagenciacaja%TYPE := 1; -- 8 es AGENCIA VIRTUAL
vPeriodoCaja        caja.periodocaja%TYPE;
vNumeroCaja         caja.numerocaja%TYPE;
--
vICodigoAgenciaCaja  caja.codigoagenciacaja%TYPE := 1; -- 8 es AGENCIA VIRTUAL
vIPeriodoCaja        caja.periodocaja%TYPE;
vINumeroCaja         caja.numerocaja%TYPE;

--
vMonedaPrestamo        cajadetalle.moneda%TYPE;
vNivelMovimiento       caja.nivelmovimiento%TYPE;
vMensaje               VARCHAR2(2000);
vSaldoImporte1         cuentacorriente.saldoimporte1%TYPE;
vSaldoImporte2         cuentacorriente.saldoimporte2%TYPE;
vDocumentoOrigen       aportes.numerodocumento%TYPE;
vFondoCaja             caja.cuentabanco%TYPE;
vMontoSoles            aportes.saldoimporte1%TYPE;
vMontoDolares          aportes.saldoimporte1%TYPE;
vTipocambio            xtipocambio.venta%TYPE;
--
BEGIN
      vNivelMovimiento := 8;
      vCodigoBanco := pkg_datosbanco.f_obt_codigobanco( PICuentaBanco );
      --
      vTipoCambio := PITipocambio;
      --
      -- vPeriodoCaja := TO_CHAR(SYSDATE, 'yyyymm');
      vPeriodoCaja := TO_CHAR(PIFechaBoleta, 'yyyymm');
      CAJ07010( vCodigoAgenciaCaja, vPeriodoCaja, 1, vNumeroCaja);
      DBMS_OUTPUT.PUT_LINE(' Datos Caja '||vCodigoAgenciaCaja||' - '||vPeriodoCaja||' - '||vNumeroCaja);
      --
      vComentario := ' RECAUDACION BANCO '||PICuentaBanco;
      --
      
      pkg_caja.p_ins_caja( pCodigoagenciacaja   => vCodigoAgenciaCaja,
                           pPeriodocaja         => vPeriodoCaja,
                           pNumerocaja          => vNumeroCaja,
                           pCuentabanco         => PICuentaBanco, -- numerocuenta segun sea el banco.
                           pTipomovimiento      => 1,
                           pTipocaja            => 1,
                           pFormapago           => 5,
                           --pTipocambio          => vTipocambio,                           
                           pTipocambio          => GEN05200(PIFechaBoleta, 3, 3),                           
                           pGlosa               => vComentario,--'RECAUDACION SEGUN SEA EL BANCO'
                           pNivelmovimiento     => vNivelMovimiento,
                           pCodigopersona       => 1,
                           pNumerocheque        => PINumerocheque,
                           pEstadoconciliacion  => 0,
                           --1d
                           pFechaoperacionbanco => PIFechaBoleta, --TRUNC(SYSDATE),
                           pFechaconciliacion   => NULL,
                           pNumeroreposicion    => NULL,
                           pNegociable          => 0,
                           pControl             => 21,
                           pEstado              => 1,
                           pPeriodolibro        => NULL,
                           pCodigolibro         => NULL,
                           pCodigousuario       => USER,
                           --David 02/01/2020                           
                           pFechausuario        => PIFechaCarga,
                           --pFechausuario        => PIFechaPagoBoleta,                           
                           pFechamovimiento     => PIFechaBoleta, --TRUNC(SYSDATE),
                           pImporte             => PIImporte,
                           pBancoorigen         => NULL,
                           pTurno               => 1,
                           pNumerochequera      => NULL,
                           pNombrecheque        => NULL,
                           pTiporeprogramacion  => NULL
                         );
      --
      IF PIMonedaCuenta = 1  THEN
         vNumerocuentaBolsa := 12117;
         vMontoSoles   := PIImporte;
         vMontoDolares := ROUND(PIImporte / vTipocambio,2);
      ElSE
          vNumerocuentaBolsa := 22889;
          vMontoSoles   := ROUND(PIImporte * vTipocambio,2);
          vMontoDolares := PIImporte;
      END IF;
      vComentario   := 'CARGA BOLETA A LA BOLSA '||vNumerocuentaBolsa;
      --
      pkg_cajadetalle.p_ins_cajadetalle( pCodigoagenciacaja => vCodigoAgenciaCaja,
                                         pPeriodocaja       => vPeriodoCaja,
                                         pNumerocaja        => vNumeroCaja,
                                         pNumeroitem        => 1,
                                         pCodigopersona     => PICodigoPersona,  -- Siempre es 1 - Cooperativa
                                         pNivelmovimiento   => vNivelMovimiento,
                                         pGrupomovimiento   => 12,
                                         pMoneda            => PIMonedaCuenta,
                                         pTipodocumento     => 8,
                                         --3d
                                         pPeriododocumento  => TO_CHAR(PIFechaBoleta, 'RRRR'), --TO_CHAR(SYSDATE, 'yyyy'),
                                         pNumerodocumento   => vNumerocuentaBolsa, -- BOLSA segun moneda
                                         pCodigoarea        => NULL,
                                         pComentario        => Substr(vComentario,1,200)                                         
                                       );
      DBMS_OUTPUT.PUT_LINE('__p_ins_cajadetalle_____PIFechapagoBoleta 0: '||TO_CHAR(PIFechaBoleta, 'yyyy')); 
      --GRABANDO EN CAJADETALLERUBROS
      pkg_cajadetalle.p_ins_cajadetallerubros ( pCodigoagenciacaja => vCodigoAgenciaCaja,
                                                pPeriodocaja       => vPeriodoCaja,
                                                pNumerocaja        => vNumeroCaja,
                                                pNumeroitem        => 1,
                                                pTipocambio        => vTipocambio,
                                                pSubimportedolares => vMontoDolares,
                                                pSubimportesoles   => vMontoSoles,
                                                pCodigomovimiento  => 30,
                                                pPeriodolibro      => NULL,
                                                pCodigolibro       => NULL,
                                                pItemlibro         => NULL
                                              );
      --
      vSaldoImporte1 := pkg_cuentacorriente.f_obt_saldoimporte1( vNumerocuentaBolsa );
      vSaldoImporte2 := pkg_cuentacorriente.f_obt_saldoimporte2( vNumerocuentaBolsa );
      --
      vSaldoImporte1 := vSaldoImporte1 + PIImporte;
      vSaldoImporte2 := vSaldoImporte2 + PIImporte;
      --
      vDocumentoOrigen := pkg_aportes.f_max_numerodocumento( vNumerocuentaBolsa );
      vDocumentoOrigen := Nvl(vDocumentoOrigen, 0) + 1;
      --
      vComentario := 'ABONO A CUENTA DE LA BOLSA '||vNumerocuentaBolsa||' - BOLETA : '||PINumerocheque;
      --GRABANDO EN APORTES
      pkg_aportes.P_INS_APORTES ( pNumerocuenta          => vNumerocuentaBolsa,
                                  pNumerodocumento       => vDocumentoOrigen,
                                  pCodigoagencia         => vCodigoAgenciaCaja,
                                  --David 02/01/2020
                                  --pFechamovimiento       => SYSDATE,
                                  pFechamovimiento       => PIFechaBoleta,                                  
                                  --pFechadisponible       => SYSDATE, --PIFechapagoBoleta 12.12.2019
                                  pFechadisponible       => PIFechaBoleta, --PIFechapagoBoleta 12.12.2019
                                  pCondicion             => 2,
                                  pTipomovimiento        => 1,
                                  pFormapago             => 5,
                                  pImporte1              => PIImporte,
                                  pSaldoimporte1         => vSaldoImporte1,
                                  pImporte2              => PIImporte,
                                  pSaldoimporte2         => vSaldoImporte2,
                                  pObservacion           => vComentario,
                                  pEstado                => 1,
                                  pCodigotransaccion     => 0,
                                  pCodigousuario         => USER,
                                  --David 02/01/2020
                                  pFechausuario          => PIFechaCarga,
                                  --pFechausuario          => PIFechaPagoBoleta,
                                  pCodigoagenciacaja     => vCodigoAgenciaCaja,
                                  pPeriodocaja           => vPeriodoCaja,
                                  pNumerocaja            => vNumeroCaja,
                                  pAporteextraordinario  => 0
                                );
      --
      UPDATE CuentaCorriente
         SET SaldoImporte1 = vSaldoImporte1,
             SaldoImporte2 = vSaldoImporte2,
             UltimoMovimiento = SYSDATE
       WHERE NumeroCuenta = vNumerocuentaBolsa;
      --
      pkg_operacionescajero.p_generarAsiento( P_NivelMovimiento => vNivelMovimiento,
                                              P_Agencia         => vCodigoAgenciaCaja,
                                              P_PeriodoCaja     => vPeriodoCaja,
                                              P_NumeroCaja      => vNumeroCaja,
                                              P_PeriodoCajaX    => NULL,
                                              P_NumeroCajaX     => NULL,
                                              P_PeriodoCajaY    => NULL,
                                              P_NumeroCajaY     => NUlL,
                                              vMensajes         => vMensaje
                                            );
    --*******************************************************************
    -- Operacion de Transferencia de la Bolsa a la cuenta del Socio
    --*******************************************************************
      vMonedaPrestamo := pkg_prestamo.f_obt_moneda( PINumerosolicitud, PIPeriodosolicitud );
      vCodigoPersonaSocio := pkg_prestamo.f_obt_codigopersona( PINumerosolicitud, PIPeriodosolicitud );
      vNivelMovimiento := 1;
      --
      --4d
      -- vIPeriodoCaja := TO_CHAR(SYSDATE, 'yyyymm'); 11.11.2019
      vIPeriodoCaja := TO_CHAR(PIFechaCarga, 'yyyymm');
      CAJ07010( vICodigoAgenciaCaja, vIPeriodoCaja, 3, vINumeroCaja);
      --
DBMS_OUTPUT.PUT_LINE(' Datos Caja 2 '||vICodigoAgenciaCaja||' - '||vIPeriodoCaja||' - '||vINumeroCaja);
      IF vMonedaPrestamo = 1 THEN
         vFondoCaja := 'FONDOCAJA-NS';
      ELSE
         vFondoCaja := 'FONDOCAJA-DO';
      END IF;
      --
      vNumerocuentaSocio := pkg_cuentacorriente.f_obt_mincuentaahv( vCodigoPersonaSocio, 2, vMonedaPrestamo );
DBMS_OUTPUT.PUT_LINE(' vCodigoPersonaSocio '||vCodigoPersonaSocio);
DBMS_OUTPUT.PUT_LINE(' vMonedaPrestamo '||vMonedaPrestamo);
DBMS_OUTPUT.PUT_LINE(' vNumerocuentaSocio '||vNumerocuentaSocio);
      IF NVL(vNumerocuentaSocio,0) = 0 THEN
         RAISE veNotieneCtaAHV;
      END IF;
      --
      vComentario := 'TRANSFERENCIA POR RECAUDACION BANCO '||PICuentaBanco||' A CUENTA DE SOCIO : '||vNumerocuentaSocio ;
      --
      DBMS_OUTPUT.PUT_LINE('PIFechaProceso: '||PIFechaBoleta);
      pkg_caja.p_ins_caja( pCodigoagenciacaja   => vICodigoAgenciaCaja,
                           pPeriodocaja         => vIPeriodoCaja,
                           pNumerocaja          => vINumeroCaja,
                           pCuentabanco         => vFondoCaja, -- numerocuenta segun sea el banco.
                           pTipomovimiento      => 3,
                           pTipocaja            => 0,
                           pFormapago           => 3,
                           pTipocambio          => vTipocambio,
                           pGlosa               => vComentario,--'RECAUDACION SEGUN SEA EL BANCO'
                           pNivelmovimiento     => vNivelMovimiento,
                           pCodigopersona       => 1,
                           pNumerocheque        => PINumerocheque,
                           pEstadoconciliacion  => 10,
                           --5d
                           pFechaoperacionbanco => NULL, --TRUNC(SYSDATE),  11.11.2019
                           pFechaconciliacion   => PIFechaCarga, -- TRUNC(SYSDATE), 11.11.2019
                           pNumeroreposicion    => NULL,
                           pNegociable          => 0,
                           pControl             => 8,
                           pEstado              => 1,
                           pPeriodolibro        => NULL,
                           pCodigolibro         => NULL,
                           pCodigousuario       => USER,
                           --David 02/01/2020
                           pFechausuario        => PIFechaCarga,
                           --pFechausuario        => PIFechaPagoBoleta,
                           --6d
                           pFechamovimiento     => PIFechaCarga, --TRUNC(SYSDATE),  11.11.2019
                           pImporte             => PIImporte,
                           pBancoorigen         => NULL,
                           pTurno               => 1,
                           pNumerochequera      => NULL,
                           pNombrecheque        => NULL,
                           pTiporeprogramacion  => NULL
                         );
                       
      --
      IF PIMonedaCuenta = 1  THEN
         vMontoSoles   := PIImporte;
         vMontoDolares := ROUND(PIImporte / vTipocambio,2);

      ElSE
          vMontoSoles   := ROUND(PIImporte * vTipocambio,2);
          vMontoDolares := PIImporte;
      END IF;
      --
      pkg_cajadetalle.p_ins_cajadetalle( pCodigoagenciacaja => vICodigoAgenciaCaja,
                                         pPeriodocaja       => vIPeriodoCaja,
                                         pNumerocaja        => vINumeroCaja,
                                         pNumeroitem        => 1,
                                         pCodigopersona     => PICodigoPersona,
                                         pNivelmovimiento   => vNivelMovimiento,
                                         pGrupomovimiento   => 4,
                                         pMoneda            => vMonedaPrestamo,
                                         pTipodocumento     => 8,
                                         --7d
                                         pPeriododocumento  => TO_CHAR(PIFechaBoleta, 'RRRR'), --TO_CHAR(SYSDATE, 'yyyy'),
                                         pNumerodocumento   => vNumerocuentaBolsa,
                                         pCodigoarea        => NULL,
                                         pComentario        => Substr(vComentario,1,200)
                                       );
      DBMS_OUTPUT.PUT_LINE('__p_ins_cajadetalle_____PIFechapagoBoleta 1: '||TO_CHAR(PIFechaBoleta, 'yyyy'));
      -- Datos de Socio
      pkg_cajadetalle.p_ins_cajadetalle( pCodigoagenciacaja => vICodigoAgenciaCaja,
                                         pPeriodocaja       => vIPeriodoCaja,
                                         pNumerocaja        => vINumeroCaja,
                                         pNumeroitem        => 2,
                                         pCodigopersona     => vCodigoPersonaSocio,
                                         pNivelmovimiento   => vNivelMovimiento,
                                         pGrupomovimiento   => 1,
                                         pMoneda            => vMonedaPrestamo,
                                         pTipodocumento     => 8,
                                         --8d   
                                         pPeriododocumento  => TO_CHAR(PIFechaBoleta, 'RRRR'), --TO_CHAR(SYSDATE, 'yyyy'),
                                         pNumerodocumento   => vNumerocuentaSocio,
                                         pCodigoarea        => NULL,
                                         pComentario        => Substr(vComentario,1,200)
                                       );
      DBMS_OUTPUT.PUT_LINE('__p_ins_cajadetalle_____PIFechapagoBoleta 2: '||TO_CHAR(PIFechaBoleta, 'yyyy'));
      --GRABANDO EN CAJADETALLERUBROS
      pkg_cajadetalle.p_ins_cajadetallerubros ( pCodigoagenciacaja => vICodigoAgenciaCaja,
                                                pPeriodocaja       => vIPeriodoCaja,
                                                pNumerocaja        => vINumeroCaja,
                                                pNumeroitem        => 1,
                                                pTipocambio        => vTipocambio,
                                                pSubimportedolares => vMontoDolares,
                                                pSubimportesoles   => vMontoSoles,
                                                pCodigomovimiento  => 58,
                                                pPeriodolibro      => NULL,
                                                pCodigolibro       => NULL,
                                                pItemlibro         => NULL
                                              );
      -- Datos de Socio
      pkg_cajadetalle.p_ins_cajadetallerubros ( pCodigoagenciacaja => vICodigoAgenciaCaja,
                                                pPeriodocaja       => vIPeriodoCaja,
                                                pNumerocaja        => vINumeroCaja,
                                                pNumeroitem        => 2,
                                                pTipocambio        => vTipocambio,
                                                pSubimportedolares => vMontoDolares,
                                                pSubimportesoles   => vMontoSoles,
                                                pCodigomovimiento  => 1,
                                                pPeriodolibro      => NULL,
                                                pCodigolibro       => NULL,
                                                pItemlibro         => NULL
                                              );
     -- Insertando en Boleta Disponible
     INSERT INTO boletadisponible( item,
                                   codigoagenciacaja,
                                   periodocaja,
                                   numerocaja,
                                   codigobanco,
                                   cuentabanco,
                                   numerocheque,
                                   fechaoperacionbanco,
                                   fechamovimiento,
                                   importe,
                                   importeusado,
                                   disponible,
                                   codigousuario,
                                   fechausuario,
                                   estado,
                                   codigoagenciacajaorigen,
                                   periodocajaorigen,
                                   numerocajaorigen )
                          VALUES ( 1,
                                   vICodigoAgenciaCaja,
                                   vIPeriodoCaja,
                                   vINumeroCaja,
                                   vCodigoBanco,
                                   vFondoCaja,
                                   PINumerocheque,
                                   PIFechaBoleta, --TRUNC(SYSDATE),
                                   PIFechaCarga, -- TRUNC(SYSDATE),
                                   PIImporte,
                                   PIImporte,
                                   0,
                                   USER,
                                   --David 02/01/2020
                                   SYSDATE,
                                   --PIFechaPagoBoleta,
                                   1,
                                   vCodigoAgenciaCaja,
                                   vPeriodoCaja,
                                   vNumeroCaja );
    -- Actualizando datos de conciliacion
    UPDATE caja
       SET estadoconciliacion = 10,
       --9d
           fechaconciliacion  = PIFechaCarga --SYSDATE
     WHERE codigoagenciacaja = vCodigoAgenciaCaja
       AND periodocaja = vPeriodoCaja
       AND numerocaja = vNumeroCaja;
    --**********************************
    -- Insertando movimiento de Aportes
    --**********************************
    vSaldoImporte1 := pkg_cuentacorriente.f_obt_saldoimporte1( vNumerocuentaBolsa );
    vSaldoImporte2 := pkg_cuentacorriente.f_obt_saldoimporte2( vNumerocuentaBolsa );
    --
    vSaldoImporte1 := vSaldoImporte1 - PIImporte;
    vSaldoImporte2 := vSaldoImporte2 - PIImporte;
    --
    vDocumentoOrigen := pkg_aportes.f_max_numerodocumento( vNumerocuentaBolsa );
    vDocumentoOrigen := Nvl(vDocumentoOrigen, 0) + 1;
    vComentario := 'SALIDA DE LA BOLSA '||vNumerocuentaBolsa||' A CUENTA SOCIO : '|| vNumerocuentaSocio;
    --GRABANDO EN APORTES
    pkg_aportes.P_INS_APORTES ( pNumerocuenta          => vNumerocuentaBolsa,
                                pNumerodocumento       => vDocumentoOrigen,
                                pCodigoagencia         => vCodigoAgenciaCaja,
                                --David 02/01/2020
                                --pFechamovimiento       => SYSDATE,
                                pFechamovimiento       => PIFechaCarga,                                
                                --pFechadisponible       => SYSDATE,  --PIFechaPagoBoleta 12.12.2019
                                pFechadisponible       => PIFechaCarga, --PIFechapagoBoleta 12.12.2019
                                pCondicion             => 2,
                                pTipomovimiento        => 4,
                                pFormapago             => 3,
                                pImporte1              => PIImporte,
                                pSaldoimporte1         => vSaldoImporte1,
                                pImporte2              => PIImporte,
                                pSaldoimporte2         => vSaldoImporte2,
                                pObservacion           => vComentario,
                                pEstado                => 1,
                                pCodigotransaccion     => 0,
                                pCodigousuario         => USER,
                                --David 02/01/2020
                                pFechausuario          => PIFechaCarga,
                                --pFechausuario          => PIFechaPagoBoleta,
                                pCodigoagenciacaja     => vICodigoAgenciaCaja,
                                pPeriodocaja           => vIPeriodoCaja,
                                pNumerocaja            => vINumeroCaja,
                                pAporteextraordinario  => 0
                              );
    --
    UPDATE CuentaCorriente
       SET SaldoImporte1 = vSaldoImporte1,
           SaldoImporte2 = vSaldoImporte2,
           --10d
           UltimoMovimiento = PIFechaBoleta --SYSDATE
     WHERE NumeroCuenta = vNumerocuentaBolsa;
    --
    vSaldoImporte1 := pkg_cuentacorriente.f_obt_saldoimporte1( vNumerocuentaSocio );
    vSaldoImporte2 := pkg_cuentacorriente.f_obt_saldoimporte2( vNumerocuentaSocio );
    --
    vSaldoImporte1 := vSaldoImporte1 + PIImporte;
    vSaldoImporte2 := vSaldoImporte2 + PIImporte;
    --
    vDocumentoOrigen := pkg_aportes.f_max_numerodocumento( vNumerocuentaSocio );
    vDocumentoOrigen := Nvl(vDocumentoOrigen, 0) + 1;
    --
    vComentario := 'ENTRADA DESDE LA BOLSA '||vNumerocuentaBolsa;
    --GRABANDO EN APORTES
    pkg_aportes.P_INS_APORTES ( pNumerocuenta          => vNumerocuentaSocio,
                                pNumerodocumento       => vDocumentoOrigen,
                                pCodigoagencia         => vCodigoAgenciaCaja,
                                --David 02/01/2020
                                --pFechamovimiento       => SYSDATE,
                                pFechamovimiento       => PIFechaCarga,                                
                                --pFechadisponible       => SYSDATE, --PIFechaPagoBoleta
                                pFechadisponible       => PIFechaCarga,
                                pCondicion             => 2,
                                pTipomovimiento        => 3,
                                pFormapago             => 3,
                                pImporte1              => PIImporte,
                                pSaldoimporte1         => vSaldoImporte1,
                                pImporte2              => PIImporte,
                                pSaldoimporte2         => vSaldoImporte2,
                                pObservacion           => vComentario,
                                pEstado                => 1,
                                pCodigotransaccion     => 0,
                                pCodigousuario         => USER,                                
                                --David 02/01/2020
                                pFechausuario          => PIFechaCarga,
                                --pFechausuario          => PIFechaPagoBoleta,
                                pCodigoagenciacaja     => vICodigoAgenciaCaja,
                                pPeriodocaja           => vIPeriodoCaja,
                                pNumerocaja            => vINumeroCaja,
                                pAporteextraordinario  => 0
                              );
      --
      UPDATE CuentaCorriente
         SET SaldoImporte1 = vSaldoImporte1,
             SaldoImporte2 = vSaldoImporte2,
             UltimoMovimiento = PIFechaBoleta --SYSDATE
       WHERE NumeroCuenta = vNumerocuentaSocio;
      --
      pkg_operacionescajero.p_generarAsiento( P_NivelMovimiento => vNivelMovimiento,
                                              P_Agencia         => vICodigoAgenciaCaja,
                                              P_PeriodoCaja     => vIPeriodoCaja,
                                              P_NumeroCaja      => vINumeroCaja,
                                              P_PeriodoCajaX    => NULL,
                                              P_NumeroCajaX     => NULL,
                                              P_PeriodoCajaY    => NULL,
                                              P_NumeroCajaY     => NUlL,
                                              vMensajes         => vMensaje
                                            );
      --
      POestadotx       := '1';
      POidoperacion    := vCodigoAgenciaCaja||vPeriodoCaja||vNumeroCaja;
      PONumeroCtaSocio := vNumerocuentaSocio;
      POmensaje        := 'TRANSACCION SATISFACTORIA';

EXCEPTION WHEN veNotieneCtaAHV  THEN
DBMS_OUTPUT.PUT_LINE(' veNotieneCtaAHV ');
          POestadotx := '0';
          POidoperacion := 0;
          PONumeroCtaSocio := NULL;
          POmensaje         := 'TRANSACCION NO SATISFACTORIA - NO TIENE CUENTA AHV '||SQLERRM;
          WHEN OTHERS THEN
DBMS_OUTPUT.PUT_LINE(' OTHERS '||sqlerrm);
          POestadotx := '0';
          POidoperacion := 0;
          PONumeroCtaSocio := NULL;
          POmensaje         := 'TRANSACCION NO SATISFACTORIA '||SQLERRM;
END P_GEN_PROCESAPAGOS;
--

PROCEDURE P_GEN_CANCELACUOTA( PITipoProceso IN NUMBER,  -- 1 Procesa Boleta Sin Pagar prestamo
                              POmensaje     OUT VARCHAR2,  -- 2 Procesa Boleta con Pago prestamo
                              PIPeriodoSolicitud IN NUMBER DEFAULT NULL,
                              PINumeroSolicitud IN NUMBER DEFAULT NULL,
                              PITipopago        IN VARCHAR2 DEFAULT NULL,
                              PINumerocuota     IN NUMBER DEFAULT NULL
                            ) IS

CURSOR cargapagos IS
SELECT TRUNC(a.fechacarga)fechacarga,
       a.usuariocarga,
       a.codigosocio,
       a.nombrecliente,
       a.referencias,
       a.importeorigen,
       a.importedepositado,
       a.importemora,
       a.oficinapago,
       a.nromovimiento,
       a.fechapago,
       a.tipopago,
       a.estado,
       a.codigobanco,
       a.numerocuentabanco,
       a.periodosolicitud,
       a.numerosolicitud,
       a.moneda,
       a.numerocuota,
       a.fechavencimiento,
       a.amortizacion,
       a.interes,
       a.mora,
       a.reajuste,
       a.portes,
       a.segurointeres,
       a.fechaproceso,
       a.usuarioproceso,
       a.trama,
       a.fechaenvio,
       a.rowid,
       a.debitoautomatico
  FROM recaudacionbanco a
 WHERE a.estado = '1' 
   AND a.periodosolicitud = PIPeriodosolicitud
   AND a.numerosolicitud  = PINumerosolicitud
   AND a.tipopago         = PITipopago
   AND a.numerocuota      = PINumerocuota;
 --AND a.debitoautomatico=PITipoProceso;
 

vCodigopersona     persona.codigopersona%TYPE;
vPeriodosolicitud  prestamo.periodosolicitud%TYPE;
vNumerosolicitud   prestamo.numerosolicitud%TYPE;
vTipocambio        cajadetallerubros.tipocambio%TYPE := GEN05200(ADM05040, 3, 3);
vComentario      VARCHAR2(500);
vNumerocuota     prestamocuotas.numerocuota%TYPE;
vFechaCalculo    prestamocuotas.fechavencimiento%TYPE;
vPagarCapital    prestamocuotas.amortizacion%TYPE;
vInteres         prestamocuotas.interes%TYPE;
vMora            prestamocuotas.interes%TYPE;
vDesgravamen     prestamocuotas.reajuste%TYPE;
vGastosCobranzas prestamocuotas.reajuste%TYPE;
vAporte          prestamocuotas.portes%TYPE;
vSeguroBien      prestamocuotas.segurointeres%TYPE;
--
vFechaCarga       recaudacionbanco.fechacarga%TYPE;

vOestadotx         VARCHAR2(500);
vOidoperacion      NUMBER;
vONumeroCtaSocio   NUMBER;
vOmensaje          VARCHAR2(500);

vO1estadotx         VARCHAR2(500);
vO1idoperacion      NUMBER;
vO1montoadebitar    NUMBER;
vO1montoadepositar  NUMBER;
vO1mensaje          VARCHAR2(500);
vMensajeLog          VARCHAR2(600):=' ';
vTramaLog          VARCHAR2(1000):=' ';
vBancoLog          VARCHAR2(2):=' ';
vCodigoSocioLog          VARCHAR2(20):=' ';
vNumeroSolicitudLog          VARCHAR2(30):=' ';
vPeriodoSolicitudLog          VARCHAR2(10):=' ';
vContadorErrores  NUMBER:=0;
vContadorCursor  NUMBER:=0;

veTipoPagoNoExiste  EXCEPTION;
veProceso  EXCEPTION; 
BEGIN
     --
     dbms_output.put_line('1' );
     FOR x IN cargapagos LOOP
        vContadorCursor:=vContadorCursor+1;
     BEGIN
        dbms_output.put_line('2' );
         vCodigopersona := pkg_persona.f_obt_codigopersona( substr(x.referencias,1,7) );
         vPeriodosolicitud := x.periodosolicitud;
         vNumerosolicitud  := x.numerosolicitud;

         dbms_output.put_line(' CIP '||substr(x.referencias,1,7));
         dbms_output.put_line(' vCodigopersona '||vCodigopersona);
         dbms_output.put_line(' vPeriodosolicitud '||vPeriodosolicitud);
         dbms_output.put_line(' vNumerosolicitud '||vNumerosolicitud);



         pkg_recaudacionbanco.p_gen_procesapagos( PICodigopersona    => vCodigopersona,
                                                  PICuentaBanco      => x.numerocuentabanco,--vCuentaBanco,
                                                  PIMonedaCuenta     => x.moneda,  --vMonedaCuenta,
                                                  PITipocambio       => vTipocambio,
                                                  PINumeroCheque     => x.nromovimiento,
                                                  PIImporte          => x.importedepositado,
                                                  PIPeriodosolicitud => vPeriodosolicitud,
                                                  PINumerosolicitud  => vNumerosolicitud,                                                  
                                                  --PIFechaPagoBoleta  => x.fechapago, 
                                                  PIFechaCarga  => x.fechacarga,
                                                  POestadotx         => vOestadotx,
                                                  POidoperacion      => vOidoperacion,
                                                  PONumeroCtaSocio   => vONumeroCtaSocio,
                                                  POmensaje          => vOmensaje,                                                  
                                                  --PIFechaPago     => x.fechacarga,    -- 02/01/2020 - David Chara I.
                                                  PIFechaBoleta     => x.fechapago,    -- 02/01/2020 - David Chara I.
                                                  PIFechaEnvio     => x.fechaenvio    -- 03/02/2020 - David Chara I.
                                                );
         dbms_output.put_line(' **********************' );
         dbms_output.put_line(' POestadotx '||vOestadotx );
         dbms_output.put_line(' POidoperacion '||vOidoperacion );
         dbms_output.put_line(' PONumeroCtaSocio '||vONumeroCtaSocio );
         dbms_output.put_line(' POmensaje '||vOmensaje );
         dbms_output.put_line(' Se proceso la boleta '||x.nromovimiento);
         
         IF TRIM(vOmensaje)!=TRIM('TRANSACCION SATISFACTORIA') THEN
            vMensajeLog := vOmensaje;
            vTramaLog := x.trama;
            vBancoLog := x.codigobanco;
            vCodigoSocioLog := x.codigosocio;                      
            vNumeroSolicitudLog := x.numerosolicitud;     
            vPeriodoSolicitudLog := x.periodosolicitud;   
            RAISE veProceso;
         END IF; 
         
         
         
         IF vOestadotx = 1 THEN
            IF PITipoProceso = 2 THEN
               vComentario      := ' CANCELACION POR RECAUDACION ';
               vNumerocuota     := x.numerocuota;               
               --vFechaCalculo    := x.fechapago;  ---x.fechavencimiento;--TRUNC(SYSDATE); -- Tiene que ser la fecha del pago               
               --vFechaCalculo    := x.fechavencimiento;--TRUNC(SYSDATE); -- Tiene que ser la fecha del pago
               vFechaCalculo    := x.fechapago;                                      
               --<I 10.03.2020 - David Chara I. - Ajustes capital cuenta atrasada
               --vPagarCapital    := pkg_prestamocuotas.F_OBT_AMORTIZACION( vNumerosolicitud, vPeriodosolicitud, vNumerocuota);
               --<F 10.03.2020 - David Chara I. - Ajustes capital cuenta atrasada
               
               --dbms_output.put_line('vPagarCapital: '||vPagarCapital);
               dbms_output.put_line('Nro solicitud: '||vNumerosolicitud || ' vPeriodosolicitud: '||vPeriodosolicitud || '  vNumerocuota: '|| vNumerocuota);
               /*vInteres         := pkg_recaudacionbanco.F_OBT_SALDOINTERES(vPeriodosolicitud, vNumerosolicitud, x.fechapago);
               vMora            := pkg_recaudacionbanco.F_OBT_SALDOMORA(vPeriodosolicitud, vNumerosolicitud, x.fechapago);*/
               IF TRIM(x.tipopago) = 'ACT' THEN
               dbms_output.put_line('2');
               dbms_output.put_line('vNumerosolicitud: '||vNumerosolicitud || ' , vPeriodosolicitud: '|| vPeriodosolicitud || '  , vNumerocuota: '||vNumerocuota);
               --
                  --<I 10.03.2020 - David Chara I. - Ajustes del capital cuendo la cuota es atrasada
                  dbms_output.put_line('capital');
                  vPagarCapital    := pkg_prestamocuotas.F_OBT_AMORTIZACION( vNumerosolicitud, vPeriodosolicitud, vNumerocuota);
                  dbms_output.put_line('capital: '|| vPagarCapital  );
                  --<F 10.03.2020 - David Chara I. - Ajustes del capital cuendo la cuota es atrasada
                  dbms_output.put_line('interes');                              
                  vInteres         := pkg_prestamocuotas.F_OBT_INTERES( vNumerosolicitud, vPeriodosolicitud, vNumerocuota );
                  dbms_output.put_line('interes'|| vInteres );
                  dbms_output.put_line('vMora');                     
                  vMora            := 0; --pkg_prestamocuotas.( p_NumeroSolicitud => ,p_PeriodoSolicitud => ,p_NumeroCuota =>  )
                  dbms_output.put_line('vMora'|| vMora);
               ELSIF TRIM(x.tipopago) = 'ATR' THEN
                   --vPagarCapital    := pkg_prestamocuotas.F_OBT_AMORTIZACION( vNumerosolicitud, vPeriodosolicitud, vNumerocuota);
                   --<I 10.03.2020 - David Chara I. - Ajustes del capital cuendo la cuota es atrasada                    
                    SELECT SUM(SALDOCAPITAL)
                    INTO vPagarCapital
                    FROM TABLE( CRE08070.DEUDACUOTASSIP(x.periodosolicitud, x.numerosolicitud, x.fechaenvio)) WHERE FECHAVENCIMIENTO <= x.fechaenvio
                    AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;                                                          
                   --<F 10.03.2020 - David Chara I. - Ajustes del capital cuendo la cuota es atrasada
                   vInteres         := pkg_recaudacionbanco.F_OBT_SALDOINTERES(vPeriodosolicitud, vNumerosolicitud, x.fechaenvio);
                   vMora            := pkg_recaudacionbanco.F_OBT_SALDOMORA(vPeriodosolicitud, vNumerosolicitud, x.fechaenvio);
                   dbms_output.put_line('3');
                   dbms_output.put_line('vPagarCapital: '||vPagarCapital);
               ELSE
               dbms_output.put_line('4');
                   vMensajeLog := vOmensaje;
                   vTramaLog := x.trama;
                   vBancoLog := x.codigobanco;
                   vCodigoSocioLog := x.codigosocio;       
                   vNumeroSolicitudLog := x.numerosolicitud;     
                   vPeriodoSolicitudLog := x.periodosolicitud;     
                   RAISE veTipoPagoNoExiste;                   
               END IF;
               vDesgravamen     := pkg_prestamocuotas.F_OBT_REAJUSTE( vNumerosolicitud, vPeriodosolicitud, vNumerocuota );
               vGastosCobranzas := 0;
               vAporte          := pkg_prestamocuotas.F_OBT_PORTES( vNumerosolicitud, vPeriodosolicitud, vNumerocuota ) ;
               vSeguroBien      := pkg_prestamocuotas.F_OBT_SEGUROINTERES( vNumerosolicitud, vPeriodosolicitud, vNumerocuota );
               vFechaCarga       := x.fechacarga;

dbms_output.put_line('Entra. p_gen_pagoprestamos' );
               pkg_recaudacionbanco.p_gen_pagoprestamos(   PICodigopersona    => vCodigopersona,
                                                           PINumerocuenta     => vONumeroCtaSocio,
                                                           PIPeriodosolicitud => vPeriodosolicitud,
                                                           PINumerosolicitud  => vNumerosolicitud,
                                                           PIImporteOrigen    => x.importeorigen, --Kenji 2020-12-03 Por Redondeo Globokas
                                                           PIImporte          => x.importedepositado,
                                                           PIPagarCapital     => vPagarCapital,
                                                           PIInteres          => vInteres,
                                                           PIMora             => vMora,
                                                           PIDesgravamen      => vDesgravamen,
                                                           PIGastosCobranzas  => vGastosCobranzas,
                                                           PIAporte           => vAporte,
                                                           PISeguroBien       => vSeguroBien,
                                                           --PIFechaCalculo     => vFechaCalculo,                                                           
                                                           PIComentario       => vComentario,
                                                           PITipoCambio       => vTipocambio,
                                                           PIFechaCarga       => vFechacarga,
                                                           POestadotx         => vO1estadotx,
                                                           POidoperacion      => vO1idoperacion,
                                                           POmontoadebitar    => vO1montoadebitar,
                                                           POmontoadepositar  => vO1montoadepositar,
                                                           POmensaje          => vO1mensaje,
                                                           PIFechaEnvio          => x.fechaenvio
                                                           --PIFechaProceso     => vFechacarga -- 02/01/2020 - David Chara I.
                                                         );
               dbms_output.put_line('Termina. p_gen_pagoprestamos' );
               dbms_output.put_line(' **********************' );
               dbms_output.put_line(' vO1estadotx '||vO1estadotx );
               dbms_output.put_line(' vO1idoperacion '||vO1idoperacion );
               dbms_output.put_line(' vO1montoadebitar '||vO1montoadebitar );
               dbms_output.put_line(' vO1montoadepositar '||vO1montoadepositar );
               dbms_output.put_line(' vO1mensaje '||vO1mensaje );
               dbms_output.put_line(' Pago de Prestamo Culminado  '||SQLERRM );
               
                IF TRIM(vO1mensaje)!=TRIM('TRANSACCION SATISFACTORIA') THEN
                    vMensajeLog := vO1mensaje;
                    vTramaLog := x.trama;
                    vBancoLog := x.codigobanco;
                    vCodigoSocioLog := x.codigosocio;
                    vNumeroSolicitudLog := x.numerosolicitud;     
                    vPeriodoSolicitudLog := x.periodosolicitud;
                    RAISE veProceso;
                END IF; 
                              
            END IF;   
         END IF;
         --
         UPDATE recaudacionbanco
            SET estado = 2,
                fechaproceso = SYSDATE,
                usuarioproceso = USER
          WHERE rowid = x.rowid;
          
          IF POmensaje !='1' THEN
            POmensaje :='4'; 
          END IF;          
         --
     EXCEPTION         
       WHEN veProceso THEN       
          vContadorErrores:=vContadorErrores+1;
          dbms_output.put_line(' ERROR veProceso '||SQLERRM );
          dbms_output.put_line(' ERROR '||SQLERRM );
          --POmensaje         := 'ERROR: '||SQLERRM;
          --POmensaje :='2';          
          IF POmensaje ='1' THEN
            POmensaje :='4';                     
          END IF;
          ROLLBACK;                                                    
          INSERT INTO RECAUDACIONBANCOLOG (FECHALOG,CODIGOUSUARIO,BANCO,CODIGOSOCIO,PERIODOSOLICITUD,NUMEROSOLICITUD,TRAMA,ERROR) VALUES
          (SYSDATE,USER,vBancoLog,vCodigoSocioLog,vPeriodoSolicitudLog,vNumeroSolicitudLog,vTramaLog,vMensajeLog);
          COMMIT;          
        WHEN veTipoPagoNoExiste THEN        
          vContadorErrores:=vContadorErrores+1;
          dbms_output.put_line(' ERROR veTipoPagoNoExiste '||SQLERRM );
          dbms_output.put_line(' ERROR '||SQLERRM );                    
          IF POmensaje ='1' THEN
            POmensaje :='4';                     
          END IF;                              
          ROLLBACK;
          INSERT INTO RECAUDACIONBANCOLOG (FECHALOG,CODIGOUSUARIO,BANCO,CODIGOSOCIO,PERIODOSOLICITUD,NUMEROSOLICITUD,TRAMA,ERROR) VALUES
          (SYSDATE,USER,vBancoLog,vCodigoSocioLog,vPeriodoSolicitudLog,vNumeroSolicitudLog,vTramaLog,vMensajeLog);
          COMMIT;                      
       WHEN OTHERS THEN       
          vContadorErrores:=vContadorErrores+1;
          dbms_output.put_line(' ERROR OTHERS '||SQLERRM );
          dbms_output.put_line(' ERROR '||SQLERRM );
          vMensajeLog :=SQLERRM;
          --POmensaje         := 'ERROR: '||SQLERRM;          
          IF POmensaje ='1' THEN
            POmensaje :='4';                     
          END IF;
          ROLLBACK;                    
          INSERT INTO RECAUDACIONBANCOLOG (FECHALOG,CODIGOUSUARIO,BANCO,CODIGOSOCIO,PERIODOSOLICITUD,NUMEROSOLICITUD,TRAMA,ERROR) VALUES
          (SYSDATE,USER,vBancoLog,vCodigoSocioLog,vPeriodoSolicitudLog,vNumeroSolicitudLog,vTramaLog,'ERROR NO CONTROLADO - '|| vMensajeLog );
          COMMIT;
        END;
     END LOOP;
     
     IF vContadorErrores=vContadorCursor THEN
        POmensaje :='2';
     END IF;
     --POmensaje :='1';
     --IF POmensaje='TRANSACCION SATISFACTORIA' THEN
     /*IF vO1mensaje='TRANSACCION SATISFACTORIA' THEN
        POmensaje        := 'OK';
     ELSE
        POmensaje         := 'ERROR';
        ROLLBACK;
     END IF;*/
     
EXCEPTION                                
        WHEN OTHERS THEN
          dbms_output.put_line(' ERROR OTHERS '||SQLERRM );
          dbms_output.put_line(' ERROR '||SQLERRM );
          vMensajeLog :=SQLERRM;
          --POmensaje         := 'ERROR: '||SQLERRM;
          POmensaje :='3';
          ROLLBACK;                    
          INSERT INTO RECAUDACIONBANCOLOG (FECHALOG,CODIGOUSUARIO,BANCO,CODIGOSOCIO,PERIODOSOLICITUD,NUMEROSOLICITUD,TRAMA,ERROR) VALUES
          (SYSDATE,USER,vBancoLog,vCodigoSocioLog,vPeriodoSolicitudLog,vNumeroSolicitudLog,vTramaLog,'ERROR NO CONTROLADO - '|| vMensajeLog );
          COMMIT;
END P_GEN_CANCELACUOTA;

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
                                PITipocambio           IN cajadetallerubros.tipocambio%TYPE,
                                PIFechaCarga           IN recaudacionbanco.fechacarga%TYPE,
                                --
                                POestadotx          OUT VARCHAR2,
                                POidoperacion       OUT NUMBER,
                                POmontoadebitar     OUT NUMBER,
                                POmontoadepositar   OUT NUMBER,
                                POmensaje           OUT VARCHAR2,--,
                                PIFechaEnvio   IN DATE
                                --PIFechaProceso      IN DATE :=SYSDATE
                              ) IS

vCodigoAgenciaCaja  caja.codigoagenciacaja%TYPE := 1; -- 8 es AGENCIA VIRTUAL
vPeriodoCaja        caja.periodocaja%TYPE;
vNumeroCaja         caja.numerocaja%TYPE;
--
vMonedaPrestamo        cajadetalle.moneda%TYPE;
vCodigoPersonaDestino  prestamo.codigopersona%TYPE;
vTransaccion           cajadetalle.grupomovimiento%TYPE;
vBancoOrigen           syst900.tblcodarg%TYPE:=NULL;
vDestino               cajadetalle.numerodocumento%TYPE;
vMonedaCuenta          cajadetalle.moneda%TYPE;
vNivelMovimiento       caja.nivelmovimiento%TYPE;
vMensaje               VARCHAR2(2000);
vSaldoImporte1         cuentacorriente.saldoimporte1%TYPE;
vSaldoImporte2         cuentacorriente.saldoimporte2%TYPE;
vDocumentoOrigen       aportes.numerodocumento%TYPE;
vFondoCaja             caja.cuentabanco%TYPE;
vMontoSoles            aportes.saldoimporte1%TYPE;
vMontoDolares          aportes.saldoimporte1%TYPE;
--
vImporte               aportes.importe1%TYPE; -- 18.10.2017 - Juan Salazar C
vImporteOrigen         aportes.importe1%TYPE;  --Kenji 2020-12-03 Por Redondeo Globokas
vTipocambio            xtipocambio.venta%TYPE;
dHoy Date;
--
vAporteextraordinario  aportes.aporteextraordinario%TYPE;

BEGIN
      dHoy := ADM05040;
      vNivelMovimiento := 1; -- 1 - Operaciones
      vMonedaCuenta   := pkg_cuentacorriente.f_obt_moneda(PINumerocuenta);
      vMonedaPrestamo := pkg_prestamo.f_obt_moneda( PINumerosolicitud, PIPeriodosolicitud );
      vCodigoPersonaDestino := pkg_prestamo.f_obt_codigopersona( PINumerosolicitud, PIPeriodosolicitud );
      vDestino := PIPeriodosolicitud||lpad( PINumerosolicitud,7,'0' );
      vTransaccion := 1;
      --
     
      --<I. 11.12.219 - Richard Rodriguez C. - Se cambio la variable dHoy por PIFechaPago
      IF NVL(PITipocambio,0) IN (0,1) THEN
         IF vMonedaCuenta != vMonedaPrestamo THEN
            vTipoCambio := CASE
                              WHEN vMonedaCuenta = 1 AND vMonedaPrestamo = 2 THEN pkg_xtipocambio.f_obt_venta(2,PIFechaCarga)
                              WHEN vMonedaCuenta = 2 AND vMonedaPrestamo = 1 THEN pkg_xtipocambio.f_obt_compra(2,PIFechaCarga)
                              ELSE pkg_xtipocambio.f_obt_compra(2,PIFechaCarga)
                           END;
         ELSE
            vTipoCambio := GEN05200(PIFechaCarga, 3, 3); 
         END IF;
      ELSE 
         IF vMonedaCuenta != vMonedaPrestamo THEN
            vTipoCambio := PITipocambio; 
         ELSE
            vTipoCambio := GEN05200(PIFechaCarga, 3, 3); 
         END IF;
      END IF;
      --<F. 11.12.219 - Richard Rodriguez C. - Se cambio la variable dHoy por PIFechaPago
 
      vPeriodoCaja := TO_CHAR(PIFechaCarga, 'yyyymm');
      CAJ07010( vCodigoAgenciaCaja, vPeriodoCaja, 3, vNumeroCaja);
      --
      IF vMonedaPrestamo = 1 THEN 
        vFondoCaja := 'FONDOCAJA-NS';                                     
      ELSE
        vFondoCaja := 'FONDOCAJA-DO';
      END IF;
      --
      INSERT INTO Caja ( CodigoAgenciaCaja, PeriodoCaja, NumeroCaja, CuentaBanco,
                         TipoMovimiento, TipoCaja, FormaPago, 
                         FechaMovimiento, TipoCambio, Importe,
                         Glosa, NivelMovimiento, CodigoPersona, NumeroCheque,
                         EstadoConciliacion, FechaOperacionBanco, FechaConciliacion, NumeroReposicion, Negociable, 
                         Control,Estado, PeriodoLibro, CodigoLibro, 
                         CodigoUsuario, FechaUsuario, NumeroChequera,TIPOREPROGRAMACION )
                         
                VALUES ( vCodigoAgenciaCaja, vPeriodoCaja, vNumeroCaja, vFondoCaja,
                         3, 0, 3,
                         --PIFechaCarga,vTipocambio, PIImporte,
                         PIFechaCarga,vTipocambio, PIImporteOrigen, --^ Kenji 2020-11-20 Por Redondeo Globokas
                         PIComentario, 1, PICodigopersona, NULL,
                         --0, SYSDATE, null, null, 0,
                         0, PIFechaCarga, null, null, 0,
                         9, 1, null, null, 
                        User, Sysdate, NULL, NULL );
                        -- User, PIFechaCarga, NULL, NULL );
      --
      IF vMonedaCuenta = 1  THEN
         --vMontoSoles   := PIImporte;
         --vMontoDolares := ROUND(PIImporte / vTipocambio,2);
         vMontoSoles   := PIImporteOrigen; --Kenji 2020-12-03 Por Redondeo Globokas
         vMontoDolares := ROUND(PIImporteOrigen / vTipocambio,2); --Kenji 2020-12-03 Por Redondeo Globokas

      ElSE
          --vMontoSoles   := ROUND(PIImporte * vTipocambio,2);
          --vMontoDolares := PIImporte;
          vMontoSoles   := ROUND(PIImporteOrigen * vTipocambio,2); --Kenji 2020-12-03 Por Redondeo Globokas
          vMontoDolares := PIImporteOrigen; --Kenji 2020-12-03 Por Redondeo Globokas
      END IF;
      --
      pkg_cajadetalle.p_ins_cajadetalle( pCodigoagenciacaja => vCodigoAgenciaCaja,
                                         pPeriodocaja       => vPeriodoCaja,
                                         pNumerocaja        => vNumeroCaja,
                                         pNumeroitem        => 1,
                                         pCodigopersona     => PICodigoPersona,
                                         pNivelmovimiento   => 1,
                                         pGrupomovimiento   => 5, --sGrupoOperaciones,
                                         pMoneda            => vMonedaCuenta,
                                         pTipodocumento     => 8, --vTipodocumento,
                                         --pPeriododocumento  => TO_CHAR(SYSDATE, 'yyyy'),
                                         pPeriododocumento  => TO_CHAR(PIFechaCarga, 'yyyy'),                                         
                                         pNumerodocumento   => PINumerocuenta,
                                         pCodigoarea        => NULL,
                                         pComentario        => Substr(PIComentario,1,200)
                                       );
      --GRABANDO EN CAJADETALLERUBROS
      pkg_cajadetalle.p_ins_cajadetallerubros ( pCodigoagenciacaja => vCodigoAgenciaCaja,
                                                pPeriodocaja       => vPeriodoCaja,
                                                pNumerocaja        => vNumeroCaja,
                                                pNumeroitem        => 1,
                                                pTipocambio        => vTipocambio, --PITipoCambio,
                                                pSubimportedolares => vMontoDolares,
                                                pSubimportesoles   => vMontoSoles,
                                                pCodigomovimiento  => 1, --nCodigoMovimiento,
                                                pPeriodolibro      => NULL,
                                                pCodigolibro       => NULL,
                                                pItemlibro         => NULL
                                              );
    
--David 03/01/2020 se modifico para aumentar un parametros fecha p_grabapagoprestamos

      --pkg_operacionescajero.p_grabapagoprestamos ( PICodigoagenciacaja    => vCodigoAgenciaCaja,
      pkg_recaudacionbanco.p_grabapagoprestamos ( PICodigoagenciacaja    => vCodigoAgenciaCaja,
                                                   PIPeriodocaja          => vPeriodoCaja,
                                                   PINumerocaja           => vNumeroCaja,
                                                   PIMoneda               => vMonedaCuenta,  --Moneda de la cuenta
                                                   PICodigopersona        => PICodigopersona,
                                                   PIPeriodosolicitud     => PIPeriodosolicitud,
                                                   PINumerosolicitud      => PINumerosolicitud,
                                                   PIImporte              => PIImporteOrigen,
                                                   PIPagarCapital         => PIPagarCapital,  -- Total Amortizacion
                                                   PIInteres              => PIInteres,       -- Total Interes
                                                   PIMora                 => PIMora,          -- Total Mora
                                                   PIDesgravamen          => PIDesgravamen,      -- Total Desgravamen
                                                   PIGastosCobranzas      => PIGastosCobranzas,              -- Total Gastos
                                                   PIAporte               => PIAporte,        -- Total Aportes
                                                   PISeguroBien           => PISeguroBien,    -- Total Seguro
                                                   PIFechaEnvio         => Trunc(PIFechaEnvio),  -- Fecha Proceso --23.05.2018 Julio Agurto - Modificacion para pago de prestamos (PRESTAMOFECHAPIVOT)
                                                   --PIFechaCalculo         => Trunc(PIFechaCarga),  -- Fecha Proceso --23.05.2018 Julio Agurto - Modificacion para pago de prestamos (PRESTAMOFECHAPIVOT)
                                                   PIFormaPago            => 3,               -- Forma Pago 3 - Transferencia Interna
                                                   PITransaccion          => vTransaccion,
                                                   PIComentario           => PIComentario,
                                                   PICodigoPersonaDestino => vCodigoPersonaDestino,
                                                   PIDestino              => vDestino, --?? --cajadetalle.numerodocumento%TYPE,
                                                   PIBancoOrigen          => vBancoOrigen, -- ?? syst900.tblcodarg%TYPE,
                                                   PITotal                => PIImporte,
                                                   PIGastoTrans           => 0,
                                                   PIGastoAdm             => 0,
                                                   PIMonedaPrestamo       => vMonedaPrestamo,
                                                   PITipocambio           => vTipocambio,                                                   
                                                   PIFechaCarga         => PIFechaCarga --, PITipocambio
                                                  );
      --
      IF vMonedaprestamo <> vMonedacuenta THEN
         IF vMonedaprestamo = 1 AND vMonedacuenta = 2 THEN
            --vImporte := ROUND ( PIImporte / vTipocambio, 2 );
            vImporte := ROUND ( PIImporteOrigen / vTipocambio, 2 ); --Kenji 2020-12-03 Por Redondeo Globokas
         ELSIF vMonedaprestamo = 2 AND vMonedacuenta = 1 THEN
               --vImporte := ROUND ( PIImporte * vTipocambio, 2 );
               vImporte := ROUND ( PIImporteOrigen * vTipocambio, 2 ); --Kenji 2020-12-03 Por Redondeo Globokas
         END IF;
      ELSE
          --vImporte := PIImporte;
          vImporte := PIImporteOrigen; --Kenji 2020-12-03 Por Redondeo Globokas
      END IF;
      --
      
      vSaldoImporte1 := pkg_cuentacorriente.f_obt_saldoimporte1( PINumerocuenta );
      vSaldoImporte2 := pkg_cuentacorriente.f_obt_saldoimporte2( PINumerocuenta );
      --
      vSaldoImporte1 := vSaldoImporte1 - vImporte;  
      vSaldoImporte2 := vSaldoImporte2 - vImporte;  
      --
      vDocumentoOrigen := pkg_aportes.f_max_numerodocumento( PINumerocuenta );
      vDocumentoOrigen := Nvl(vDocumentoOrigen, 0) + 1;
      --GRABANDO EN APORTES
      IF PIPeriodoSolicitud = 1 THEN
        vAporteExtraordinario:=3;
      ELSE
        vAporteExtraordinario:=0;
      END IF;
      
      pkg_aportes.P_INS_APORTES ( pNumerocuenta          => PINumerocuenta,
                                  pNumerodocumento       => vDocumentoOrigen,
                                  pCodigoagencia         => vCodigoAgenciaCaja,
                                  /*pFechamovimiento       => SYSDATE,
                                  pFechadisponible       => SYSDATE,*/
                                  pFechamovimiento       => PIFechaCarga,
                                  --pFechadisponible       => PIFechaCalculo,
                                  pFechadisponible       => PIFechaCarga,
                                  pCondicion             => 5, 
                                  pTipomovimiento        => 4,
                                  pFormapago             => 3, 
                                  pImporte1              => vImporte, 
                                  pSaldoimporte1         => vSaldoImporte1,
                                  pImporte2              => vImporte, 
                                  pSaldoimporte2         => vSaldoImporte2,
                                  pObservacion           => 'Pago prestamo'||PIPeriodosolicitud||'-'||PINumerosolicitud||' '||PIComentario,
                                  pEstado                => 1,
                                  pCodigotransaccion     => 0,
                                  pCodigousuario         => USER,
                                  --pFechausuario          => SYSDATE,
                                  pFechausuario          => SYSDATE,
                                  pCodigoagenciacaja     => vCodigoAgenciaCaja,
                                  pPeriodocaja           => vPeriodoCaja,
                                  pNumerocaja            => vNumeroCaja,
                                  pAporteextraordinario  => vAporteExtraordinario
                                );
      --
      UPDATE CuentaCorriente
         SET SaldoImporte1 = vSaldoImporte1,
             SaldoImporte2 = vSaldoImporte2,
             UltimoMovimiento = SYSDATE            
       WHERE NumeroCuenta = PINumerocuenta;
      --
      pkg_operacionescajero.p_generarAsiento( P_NivelMovimiento => vNivelMovimiento, --:Block1.NivelMovimiento,
                                              P_Agencia         => vCodigoAgenciaCaja,         -- :Block1.Agencia,
                                              P_PeriodoCaja     => vPeriodoCaja,     -- :Block1.PeriodoCaja,
                                              P_NumeroCaja      => vNumeroCaja,      -- :Block1.NumeroCaja,
                                              P_PeriodoCajaX    => NULL,              -- :Block1.PeriodoCajaX,
                                              P_NumeroCajaX     => NULL,              -- :Block1.NumeroCajaX,
                                              P_PeriodoCajaY    => NULL,              -- :Block1.PeriodoCajaY,
                                              P_NumeroCajaY     => NUlL,              -- :Block1.NumeroCajaY,
                                              vMensajes         => vMensaje
                                            );
    POestadotx := '1';
    POidoperacion := vCodigoAgenciaCaja||vPeriodoCaja||vNumeroCaja;
    --POmontoadebitar  := PIImporte;
    POmontoadebitar  := PIImporteOrigen; --Kenji 2020-12-03 Por Redondeo Globokas
    POmontoadepositar := PIImporte;
    POmensaje         := 'TRANSACCION SATISFACTORIA';

EXCEPTION WHEN OTHERS THEN
          POestadotx := '0';
          POidoperacion := 0;
          POmontoadebitar  := 0;
          POmontoadepositar := 0;
          POmensaje         := 'TRANSACCION NO SATISFACTORIA '||SQLERRM;
          
END P_GEN_PAGOPRESTAMOS;
--
FUNCTION F_OBT_SALDOINTERES( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                             PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                             PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                                ) RETURN NUMBER IS
vSaldoInteres      NUMBER;

BEGIN
  
  SELECT SUM(SALDOINTERES)
  INTO vSaldoInteres
  FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodosolicitud, PINumerosolicitud, PIFechaPago )) WHERE FECHAVENCIMIENTO <= PIFechaPago 
  AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
 
 RETURN vSaldoInteres;
     
END F_OBT_SALDOINTERES;
--

FUNCTION F_OBT_SALDOMORA( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                          PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                          PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                                ) RETURN NUMBER IS
vSaldoMora      NUMBER;
vDescProducto   syst902.tbldescri%TYPE;
BEGIN
  
  vDescProducto := PKG_SYST902.F_OBT_TBLDESCRI(PKG_PRESTAMODETALLE.F_OBT_TIPOSOLICITUD(PIPeriodosolicitud,PINumerosolicitud ,0),
                                                          PKG_PRESTAMODETALLE.F_OBT_TIPOPRESTAMO(PIPeriodosolicitud,PINumerosolicitud , 0));
               IF SUBSTR(vDescProducto,1,3) = 'PCT' THEN
                SELECT SUM(SALDOMORA)
                INTO vSaldoMora 
                FROM TABLE( PKG_TECHO_PROPIO.F_OBT_DEUDAPCT(PIPeriodosolicitud, PINumerosolicitud , PIFechaPago))
                WHERE (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
               ELSE
                SELECT SUM(SALDOMORA)
                INTO vSaldoMora 
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodosolicitud, PINumerosolicitud, PIFechaPago)) WHERE FECHAVENCIMIENTO <= PIFechaPago
                AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;-- Para que no considere los saldos en cero
               END IF;
 
 RETURN vSaldoMora;
     
END F_OBT_SALDOMORA;
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
                                 PIFechaEnvio         IN DATE,      -- Fecha Proceso
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
                                 PIFechaCarga         IN DATE := SYSDATE      --David Chara I. 03/01/2020
                                ) IS

CURSOR GASTOS IS
  Select Item, SaldoImporte,categoria, tipopago, codigopago
  From PrestamoPagoVarios
  Where PeriodoSolicitud = PIPeriodoSolicitud
  And NumeroSolicitud = PINumeroSolicitud
  And SaldoImporte > 0
  Order By FechaMovimiento, Item;

rInteres prestamo.iintere%TYPE := 0;
rMora    prestamo.imora%TYPE := 0;
rCapital prestamo.saldoprestamo%TYPE := 0;
tMENSAJE VARCHAR2(2000);

  dHoy Date;
  nTipoCambio XTipoCambio.Compra%Type;
  nPeriodoCaja Caja.NumeroCaja%Type;
  nNumeroCaja Caja.NumeroCaja%Type;
  vGlosa Caja.Glosa%Type;
  nNumeroItem CajaDetalle.NumeroItem%Type;
  nSaldoImporte1 CuentaCorriente.SaldoImporte1%Type;
  nSaldoImporte2 CuentaCorriente.SaldoImporte2%Type;
  nMontoSoles CajaDetalleRubros.SubImporteSoles%Type;
  nMontoDolares CajaDetalleRubros.SubImporteDolares%Type;
  nItemPrestamo PrestamoPagos.NumeroItem%Type;
  nNumeroCuenta CuentaCorriente.NumeroCuenta%Type;
  nAporte PrestamoCuotas.Amortizacion%Type;
  nLoopImportes Number;
  nImporte PrestamoCuotas.Amortizacion%Type;
  nImporteTrans PrestamoCuotas.Amortizacion%Type;
  nCodigoMovimiento CodigoOperaciones.CodigoMovimiento%Type;
  nControl Number(3);
  xxTipoMovimiento number;
  xGastoAdm Number;
  xGastoTrans Number;
  pAmortizacion Number;
  pInteres Number;
  pMora Number;
  pDesgravamen Number;
  pGastos Number;
  sGrupoOperaciones Number;
  nImporteAmortizado Number;
  nSaldoGasto Number;
  cDescripcion Varchar2(200);
  nImporteTransNeto Number;
  nMoneda1 number;
  nPItem       PrestamoPagoVarios.Item%TYPE;
  ncategoria   PrestamoPagoVarios.categoria%TYPE;
  ntipopago    PrestamoPagoVarios.tipopago%TYPE;
  ncodigopago  PrestamoPagoVarios.codigopago%TYPE;
  nItemPago    PrestPagoVariosDetalle.ItemPago%TYPE;

  vTipodocumento   cajadetalle.tipodocumento%TYPE;
  vNumerodocumento cajadetalle.numerodocumento%TYPE;
  nNumeroDocumento NUMBER;
  nDiasBanco       NUMBER(3);
  vMensaje         VARCHAR2(200);
  ----
  vExistetp        NUMBER;
  vTiposolicitud   NUMBER;
  vTipoprestamo    NUMBER;
BEGIN
       --<I.29.11.2019 Para Generar pago de Inscripcion en cuotas-Techo Propio- Richard Rodriguez C >
      SELECT COUNT(*)
      INTO vExistetp 
      FROM viviendaptp
      WHERE codigopersona=PICodigopersona;
      
     IF vExistetp=1 THEN
        BEGIN
        SELECT tiposolicitud,tipoprestamo
        INTO vTiposolicitud,vTipoprestamo  
        FROM solicitudprestamo 
        WHERE periodosolicitud=PIPeriodoSolicitud
        AND numerosolicitud=PINumeroSolicitud;
        EXCEPTION WHEN OTHERS THEN
          NULL;
        END;
      END IF;    
       --<F.29.11.2019 Para Generar pago de Inscripcion en cuotas-Techo Propio- Richard Rodriguez C >
        
        nNumeroItem := 1; -- se asume que el primero es el total de la operacion, aqui se cuenta a partir del 2do item

        dHoy := ADM05040;
        nTipoCambio := PITipocambio;

        Begin
          nImporteTrans := PIImporte; --:Movimientos.ImporteMonedaPrestamo;
          --
          If PIMoneda = 1 Then
             nMontoSoles := PIImporte;
             nMontoDolares := Round(PIImporte / nTipoCambio, 2);
          Else
              nMontoDolares := PIImporte;
              nMontoSoles := Round(PIImporte * nTipoCambio, 2);
          End If;
          --Inicio Ricardo Mata 08/09/2016 : Para los casos de reconocimiento de interes para el STOCK, se debe restar lo calculado de la nota
          If dHoy != ADM05040 Then
            DBMS_OUTPUT.PUT_LINE('Entra CRE07390: '|| pAmortizacion);
            CRE07390( PIPeriodoSolicitud,
                       PINumeroSolicitud,
                       dHoy,
                       ADM05040,
                       PIImporte,
                       rInteres,
                       rMora,
                       rCapital,
                       tMENSAJE );
          End If;
          --Fin Ricardo Mata 08/09/2016 : Para los casos de reconocimiento de interes para el STOCK, se debe restar lo calculado de la nota
          nAporte := Nvl(PIAporte,0);
          --
          If Nvl(PIGastoAdm,0) > 0 Then
            xGastoAdm := Round(PIGastoAdm * (nImporteTrans / PITotal), 2);
          End If;
          --
          If Nvl(PIGastoTrans,0) > 0 Then
            xGastoTrans := Round(PIGastoTrans * (nImporteTrans / PITotal), 2);
          End If;
          --
          nImporteTransNeto := Nvl(nImporteTrans,0);
          nImporteTransNeto := nImporteTransNeto - (Nvl(xGastoAdm,0) + Nvl(xGastoTrans,0) + Nvl(nAporte,0));
          --
          
          DBMS_OUTPUT.PUT_LINE('1: '|| nImporteTransNeto || '  2: '||PIPagarCapital || '  3: '||PIInteres||' 4: '||PIMora|| ' 5: '||PIDesgravamen||'  6:' || PIGastosCobranzas);
          ADM07050( nImporteTransNeto,
                    PIPagarCapital, --:Movimientos.SaldoPrestamo,
                    PIInteres,
                    PIMora,
                    PIDesgravamen,
                    PIGastosCobranzas,
                    pAmortizacion,
                    pInteres,
                    pMora,
                    pDesgravamen,
                    pGastos
                  );
          --
          vGlosa := Substr('Transf. '||PIComentario, 1, 100);
          --
          UPDATE Caja
             SET Glosa = vGlosa
           WHERE CodigoAgenciaCaja = PICodigoAgenciaCaja
             AND PeriodoCaja = PIPeriodoCaja
             AND NumeroCaja  = PINumeroCaja;
          --
          
          DBMS_OUTPUT.PUT_LINE('Amortizacion: '|| pAmortizacion);
          PKG_RECAUDACIONBANCO.CRE07132( PIPeriodoSolicitud,
          --SISGODBA.CRE07132( PIPeriodoSolicitud,
                    PINumeroSolicitud,                    
                    PIFechaCarga,
                    3, -- tipo mov.
                    pAmortizacion,
                    pInteres,
                    0,
                    pMora,
                    0,
                    2, -- cond
                    Null, -- glosa
                    pDesgravamen,
                    Null, -- obs
                    0, -- transf garant
                    PIFechaEnvio,
                    PICodigoAgenciaCaja,
                    PIFormaPago,
                    PIAporte,
                    PISeguroBien,
                    PICodigoAgenciaCaja,
                    PIPeriodoCaja,
                    PINumeroCaja,
                    nItemPrestamo
                    );
        End;
        -- Inserta en cajadetalle
        nMoneda1 := PKG_PRESTAMO.F_OBT_MONEDA(PINumeroSolicitud, PIPeriodoSolicitud);
        Begin
          nLoopImportes := 0;
          --
          Loop
            Begin
              nLoopImportes := nLoopImportes + 1;
              --
              If nLoopImportes > 9 Then
                Exit;
              End If;

              -- Da Segun Linea del Bloque Movimientos el importe y de que se trata
              If nLoopImportes = 1 Then ---capital al dia de hoy
                nImporte          := pAmortizacion;
                --<I.29.11.2019 Para Generar pago de Inscripcion en cuotas-Techo Propio- Richard Rodriguez C >
                 IF vExistetp=1 AND vTiposolicitud=900 AND vTipoprestamo = pkg_syst900.f_obt_tblcodarg(389,vTipoprestamo) THEN
                 nCodigoMovimiento := 20;
                 ELSE
                  nCodigoMovimiento := 5;
                 END IF;
                 --<F.29.11.2019 Para Generar pago de Inscripcion en cuotas-Techo Propio- Richard Rodriguez C >
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 2 Then ---interes del dia de hoy
                nImporte          := pInteres;
                nCodigoMovimiento := 6;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 3 Then  ---mora al dia de hoy
                nImporte          := pMora;
                nCodigoMovimiento := 7;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 4 Then  ---gastos administrativos
                nImporte          := Nvl(xGastoAdm,0);
                nCodigoMovimiento := 8;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 5 Then   ---gastos de tranferencia
                nImporte          := Nvl(xGastoTrans,0);
                nCodigoMovimiento := 9;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 6 Then  ---gastos de aportes
                nImporte          := PIAporte; --Ricardo Mata
                nCodigoMovimiento := 66;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 7 Then  --- saldos de desgravamen de hoy
                nImporte          := pDesgravamen; --jlaw
                nCodigoMovimiento := 63;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 8 Then  --- gastos
                nImporte          := pGastos; --jlaw agastos
                nCodigoMovimiento := 64;
                sGrupoOperaciones := PITransaccion;
              ElsIf nLoopImportes = 9 Then  --- Seguro Bien
                nImporte          := PISeguroBien; --Ricardo Mata
                nCodigoMovimiento := 17;
                sGrupoOperaciones := PITransaccion;
              End If;
              -- convierte importes a moneda de cuenta -- 14/11/08 dml
              If Nvl(PIMonedaPrestamo, PIMoneda) != PIMoneda Then
                If PIMoneda = 1 Then
                  nImporte := Round(nImporte * nTipoCambio,2);
                Else
                  nImporte := Round(nImporte / nTipoCambio,2);
                End If;
              End If;
              -- Calculo de los importes de acuerdo a la moneda
              If PIMoneda = 1 Then
                nMontoSoles := nImporte;
                nMontoDolares := Round(nImporte / nTipoCambio, 2);
              Else
                nMontoDolares := nImporte;
                nMontoSoles := Round(nImporte * nTipoCambio, 2);
              End If;
              --
              If Nvl(nImporte,0) > 0 Then
                --
                If nLoopImportes = 6 Then --- AMORTIZA APORTES ---   ---- aportes
                  --
                   BEGIN
                     SELECT nvl(min(NumeroCuenta),0)
                     INTO nNumeroCuenta
                     FROM CuentaCorriente
                     WHERE TipoTransaccion = 1 AND ESTADO = 1 and moneda = nMoneda1
                     AND CodigoPersona = PICodigoPersonaDestino;
                  EXCEPTION WHEN NO_DATA_FOUND THEN   nNumeroDocumento := 1;
                  END;
                  --
                  nNumeroDocumento := pkg_aportes.f_max_numerodocumento(nNumeroCuenta);
                  nNumeroDocumento := Nvl(nNumeroDocumento, 0) + 1;
                  --
                  nSaldoImporte1 := pkg_cuentacorriente.f_obt_saldoimporte1( nNumeroCuenta );
                  nSaldoImporte2 := pkg_cuentacorriente.f_obt_saldoimporte2( nNumeroCuenta );
                  --
                  IF PIFormaPago = 2 THEN
                     BEGIN
                          SELECT Nvl(To_Number(TblDesAbr),0)
                            INTO nDiasBanco
                            FROM Syst900
                           WHERE TblCodTab = 39
                             AND TblCodArg = PIBancoOrigen;
                     EXCEPTION WHEN No_Data_Found THEN
                               nDiasBanco := 0;
                     END;
                     --
                     IF Nvl(nDiasBanco,0) = 0 THEN
                        nSaldoImporte1 := nSaldoImporte1 + nImporte;
                        nSaldoImporte2 := nSaldoImporte2 + nImporte;
                     ELSE
                         nSaldoImporte1 := nSaldoImporte1 + nImporte;
                     END IF;
                  ELSE
                       nSaldoImporte1 := nSaldoImporte1 + nImporte;
                       nSaldoImporte2 := nSaldoImporte2 + nImporte;
                  END IF;
                  --
                  If PIMoneda = 1 Then
                    nMontoSoles   := nImporte;
                    nMontoDolares := Round(nImporte / nTipoCambio, 2);
                    --
                    IF nControl = 11 THEN
                      xxTipoMovimiento := 2;
                    Else
                      xxTipoMovimiento := 1;
                    End if;
                  Else
                    nMontoDolares := nImporte;
                    nMontoSoles   := Round(nImporte * nTipoCambio, 2);
                    IF nControl = 11 THEN
                      xxTipoMovimiento := 1;
                    Else
                      xxTipoMovimiento := 2;
                    End If;
                  End If;
                  --
                  --GRABANDO EN APORTES
                  pkg_aportes.P_INS_APORTES ( pNumerocuenta          => nNumeroCuenta,
                                              pNumerodocumento       => nNumeroDocumento,
                                              pCodigoagencia         => PICodigoAgenciaCaja,
                                              -- David 03/01/2020 modificacion de fecha
                                              /*pFechamovimiento       => SYSDATE,
                                              pFechadisponible       => SYSDATE,*/
                                              pFechamovimiento       => PIFechaCarga,
                                              pFechadisponible       => PIFechaCarga,
                                              pCondicion             => 2,
                                              pTipomovimiento        => 3, --xxTipoMovimiento,
                                              pFormapago             => PIFormaPago,
                                              pImporte1              => nImporte,
                                              pSaldoimporte1         => nSaldoImporte1,
                                              pImporte2              => nImporte,
                                              pSaldoimporte2         => nSaldoImporte2,
                                              pObservacion           => 'Amort. Aportes por Prestamo',
                                              pEstado                => 1,
                                              pCodigotransaccion     => 0,
                                              pCodigousuario         => USER,
                                              -- David 03/01/2020 modificacion de fecha
                                              pFechausuario          => PIFechaCarga,
                                              --pFechausuario          => PIFechaProceso,
                                              pCodigoagenciacaja     => PICodigoAgenciaCaja,
                                              pPeriodocaja           => PIPeriodoCaja,
                                              pNumerocaja            => PINumeroCaja,
                                              pAporteextraordinario  => 0
                                            );
                  --
                  Begin
                    --ACTUALIZANDO CUENTA CORRIENTE
                    Update CuentaCorriente
                    Set    SaldoImporte1 = nSaldoImporte1,
                    SaldoImporte2 = nSaldoImporte2,
                    UltimoMovimiento = dHoy
                    Where  NumeroCuenta = nNumeroCuenta;
                  End;
                End If; ------ fin de aportes
                ---
                --<I.29.11.2019 Para Generar pago de Inscripcion en cuotas-Techo Propio- Richard Rodriguez C >
                IF nLoopImportes=1 AND vExistetp=1 AND vTiposolicitud=900 AND vTipoprestamo=pkg_syst900.f_obt_tblcodarg(389,vTipoprestamo) THEN
                  BEGIN
                     SELECT nvl(min(NumeroCuenta),0)
                     INTO nNumeroCuenta
                     FROM CuentaCorriente
                     WHERE TipoTransaccion = 1 AND ESTADO = 1 and moneda = nMoneda1
                     AND CodigoPersona = PICodigoPersonaDestino;
                  EXCEPTION WHEN NO_DATA_FOUND THEN   
                    nNumeroDocumento := 1;
                  END;
                  --
                  nNumeroDocumento := pkg_aportes.f_max_numerodocumento(nNumeroCuenta);
                  nNumeroDocumento := Nvl(nNumeroDocumento, 0) + 1;
                  --
                  nSaldoImporte1 := pkg_cuentacorriente.f_obt_saldoimporte1( nNumeroCuenta );
                  nSaldoImporte2 := pkg_cuentacorriente.f_obt_saldoimporte2( nNumeroCuenta );
                  --
                  IF PIFormaPago = 2 THEN
                     BEGIN
                          SELECT Nvl(To_Number(TblDesAbr),0)
                            INTO nDiasBanco
                            FROM Syst900
                           WHERE TblCodTab = 39
                             AND TblCodArg = PIBancoOrigen;
                     EXCEPTION WHEN No_Data_Found THEN
                               nDiasBanco := 0;
                     END;
                     --
                     IF Nvl(nDiasBanco,0) = 0 THEN
                        nSaldoImporte1 := nSaldoImporte1 + nImporte;
                        nSaldoImporte2 := nSaldoImporte2 + nImporte;
                     ELSE
                         nSaldoImporte1 := nSaldoImporte1 + nImporte;
                     END IF;
                  ELSE
                       nSaldoImporte1 := nSaldoImporte1 + nImporte;
                       nSaldoImporte2 := nSaldoImporte2 + nImporte;
                  END IF;
                  --
                  If PIMoneda = 1 Then
                    nMontoSoles   := nImporte;
                    nMontoDolares := Round(nImporte / nTipoCambio, 2);
                    --
                    IF nControl = 11 THEN
                      xxTipoMovimiento := 2;
                    Else
                      xxTipoMovimiento := 1;
                    End if;
                  Else
                    nMontoDolares := nImporte;
                    nMontoSoles   := Round(nImporte * nTipoCambio, 2);
                    IF nControl = 11 THEN
                      xxTipoMovimiento := 1;
                    Else
                      xxTipoMovimiento := 2;
                    End If;
                  End If;
                  --
                  --GRABANDO EN APORTES
                  pkg_aportes.P_INS_APORTES ( pNumerocuenta          => nNumeroCuenta,
                                              pNumerodocumento       => nNumeroDocumento,
                                              pCodigoagencia         => PICodigoAgenciaCaja,                                              
                                              -- David 03/01/2020 modificacion de fecha                                               
                                              pFechamovimiento       => PIFechaCarga,
                                              pFechadisponible       => PIFechaCarga,
                                              /*pFechamovimiento       => SYSDATE,
                                              pFechadisponible       => SYSDATE,*/
                                              pCondicion             => 2,
                                              pTipomovimiento        => 3, --xxTipoMovimiento,
                                              pFormapago             => PIFormaPago,
                                              pImporte1              => nImporte,
                                              pSaldoimporte1         => nSaldoImporte1,
                                              pImporte2              => nImporte,
                                              pSaldoimporte2         => nSaldoImporte2,
                                              pObservacion           => 'Amort. Aportes por Prestamo',
                                              pEstado                => 1,
                                              pCodigotransaccion     => 0,
                                              pCodigousuario         => USER,
                                              -- David 03/01/2020 modificacion de fecha                                               
                                              pFechausuario          => PIFechaCarga,
                                              --pFechausuario          => PIFechaProceso,                                              
                                              pCodigoagenciacaja     => PICodigoAgenciaCaja,
                                              pPeriodocaja           => PIPeriodoCaja,
                                              pNumerocaja            => PINumeroCaja,
                                              pAporteextraordinario  => 3
                                            );
                  --
                  Begin
                    --ACTUALIZANDO CUENTA CORRIENTE
                    Update CuentaCorriente
                    Set    SaldoImporte1 = nSaldoImporte1,
                    SaldoImporte2 = nSaldoImporte2,
                    UltimoMovimiento = dHoy
                    Where  NumeroCuenta = nNumeroCuenta;
                  End;
                END IF;
              --<F.29.11.2019 Para Generar pago de Inscripcion en cuotas-Techo Propio- Richard Rodriguez C >
                
                If nLoopImportes <> 8 and nImporte > 0 Then -- inserta otros conceptos diferentes a gastos
                    nNumeroItem := nvl(nNumeroItem,0) + 1;
                    --
                    SELECT Decode(nLoopImportes, 6, 8, 9) INTO vTipodocumento FROM DUAL;
                    SELECT Decode(nLoopImportes, 4, 0, 5, 0, 6, nNumeroCuenta, PIDestino) INTO vNumerodocumento FROM DUAL;
                    --GRABANDO EN CAJADETALLE
                    pkg_cajadetalle.p_ins_cajadetalle( pCodigoagenciacaja => PICodigoAgenciaCaja,
                                                       pPeriodocaja       => PIPeriodoCaja,
                                                       pNumerocaja        => PINumeroCaja,
                                                       pNumeroitem        => nNumeroItem,
                                                       pCodigopersona     => PICodigoPersona,
                                                       pNivelmovimiento   => 1,
                                                       pGrupomovimiento   => sGrupoOperaciones,
                                                       pMoneda            => PIMoneda,
                                                       pTipodocumento     => vTipodocumento,
                                                       --pPeriododocumento  => To_Char(dHoy, 'yyyy'), -- David Chara I. 03/01/2020 
                                                       pPeriododocumento  => To_Char(PIFechaCarga, 'yyyy'),
                                                       pNumerodocumento   => vNumerodocumento,
                                                       pCodigoarea        => NULL,
                                                       pComentario        => Substr(PIComentario,1,200)
                                                     );
                    --GRABANDO EN CAJADETALLERUBROS
                    pkg_cajadetalle.p_ins_cajadetallerubros ( pCodigoagenciacaja => PICodigoAgenciaCaja,
                                                              pPeriodocaja       => PIPeriodoCaja,
                                                              pNumerocaja        => PINumeroCaja,
                                                              pNumeroitem        => nNumeroItem,
                                                              pTipocambio        => nTipoCambio,
                                                              pSubimportedolares => nMontoDolares,
                                                              pSubimportesoles   => nMontoSoles,
                                                              pCodigomovimiento  => nCodigoMovimiento,
                                                              pPeriodolibro      => NULL,
                                                              pCodigolibro       => NULL,
                                                              pItemlibro         => NULL
                                                            );
                End If; --- fin deloop <> 8
                --
                If nLoopImportes = 8 Then -- inserta otros conceptos diferentes a gastos
                  Begin
                    nImporteAmortizado := nImporte;  -- Gastos a pagarse a fecha de hoy
                    -- Cursor de gastos
                    Open Gastos;
                    Loop
                      Begin
                        Fetch Gastos Into nPItem, nSaldoGasto, ncategoria, ntipopago, ncodigopago;
                        Exit When Gastos%NotFound Or nImporteAmortizado = 0;
                        --Busca en el cursor los registrsao de gastos
                        If nSaldoGasto < nImporteAmortizado Then
                          nSaldoGasto := nSaldoGasto;
                        Else
                          nSaldoGasto := nImporteAmortizado;
                        End If;
                        --
                        nImporteAmortizado := nImporteAmortizado - nSaldoGasto;
                        --
                        -- Actualiza los montos a colocar
                        -- Calculo de los importes de acuerdo a la moneda
                        If PIMoneda = 1 Then
                          nMontoSoles := nSaldoGasto;
                          nMontoDolares := Round(nSaldoGasto / nTipoCambio, 2);
                        Else
                          nMontoDolares := nSaldoGasto;
                          nMontoSoles := Round(nSaldoGasto * nTipoCambio, 2);
                        End If;
                        -- Descripcion de los pagos de los gastos detalles
                        Begin
                          Select Upper(Descripcion)
                          Into   cDescripcion
                          From   Pago
                          Where  Categoria  = nCategoria
                          And  TipoPago   = ntipopago
                          And  CodigoPago = ncodigopago;
                        Exception When Others Then cDescripcion := 'GASTOS';
                        End;
                        --
                        nNumeroItem := NVL(nNumeroItem,0) + 1;
                        --
                        pkg_cajadetalle.p_ins_cajadetalle( pCodigoagenciacaja => PICodigoAgenciaCaja,
                                                           pPeriodocaja       => PIPeriodoCaja,
                                                           pNumerocaja        => PINumeroCaja,
                                                           pNumeroitem        => nNumeroItem,
                                                           pCodigopersona     => PICodigoPersona,
                                                           pNivelmovimiento   => 1,
                                                           pGrupomovimiento   => sGrupoOperaciones,
                                                           pMoneda            => PIMoneda,
                                                           pTipodocumento     => 9,                                                           
                                                           --pPeriododocumento  => To_Char(dHoy, 'yyyy'), -- David Chara I. 03/01/2020 
                                                           pPeriododocumento  => To_Char(PIFechaCarga, 'yyyy'),
                                                           pNumerodocumento   => PIDestino,
                                                           pCodigoarea        => NULL,
                                                           pComentario        => Substr(cDescripcion,1,30)
                                                         );
                        --
                        --GRABANDO EN CAJADETALLERUBROS
                        pkg_cajadetalle.p_ins_cajadetallerubros ( pCodigoagenciacaja => PICodigoAgenciaCaja,
                                                                  pPeriodocaja       => PIPeriodoCaja,
                                                                  pNumerocaja        => PINumeroCaja,
                                                                  pNumeroitem        => nNumeroItem,
                                                                  pTipocambio        => nTipoCambio,
                                                                  pSubimportedolares => nMontoDolares,
                                                                  pSubimportesoles   => nMontoSoles,
                                                                  pCodigomovimiento  => nCodigoMovimiento,
                                                                  pPeriodolibro      => NULL,
                                                                  pCodigolibro       => NULL,
                                                                  pItemlibro         => NULL
                                                                );
                        --
                        Begin
                          --GRABANDO EN PRESTPAGOSVARIOSDETALLE
                          Select Nvl(Max(ItemPago),0) + 1
                          Into nItemPago
                          From PrestPagoVariosDetalle
                          Where PeriodoSolicitud = PIPeriodoSolicitud
                          And NumeroSolicitud = PINumeroSolicitud
                          And Item = nPItem;

                          Insert Into PrestPagoVariosDetalle (periodosolicitud, numerosolicitud, item, itempago, codigoagencia,
                          condicion, tipomovimiento, formapago, fechapago,
                          importe, codigousuario, fechausuario,
                          codigoagenciacaja, periodocaja, numerocaja, NumeroItem)
                          Values (PIPeriodoSolicitud, PINumeroSolicitud, nPItem, nItemPago, PICodigoAgenciaCaja,
                          2, 1, PIFormaPago,dHoy,
                          nMontoSoles, User, PIFechaCarga, PiCodigoAgenciaCaja, nPeriodoCaja, nNumeroCaja, nNumeroItem);
                        End;
                      End;
                    End Loop;
                    --
                    CAJ07020( PIPeriodoSolicitud, PINumeroSolicitud,nImporte,vMensaje);
                    --
                    If vMensaje Is Not Null Then
                       Return;
                    End If;
                  End;
                End If;
                -----------------------------------------------------------------------------------------
                --
                If nCodigoMovimiento Not In (9) Then
                  If nCodigoMovimiento = 69 then
                    nImporte := nImporte * -1;
                  end if;
                  --
                  If PIFormaPago In (1) Then
                    --P_ActualizaCajero ('I', nImporte, PIMoneda, vMensaje);
                    PKG_RECAUDACIONBANCO.p_act_ActualizaCajero ('I', nImporte,
                                 PIMoneda,
                                 PICodigoAgenciaCaja,
                                 PIFechaEnvio,
                                 vMensaje);

                    --
                    If vMensaje Is Not Null Then
                      Return;
                    End If;
                  End If;
                End If;
              End If;
            End;
          End Loop;
        End;
END P_GRABAPAGOPRESTAMOS;


PROCEDURE P_ACT_ACTUALIZACAJERO ( pOperacion VARCHAR2,
                                  pImporte   NUMBER,
                                  pMoneda    NUMBER,
                                  pAgencia   NUMBER,
                                  pFecha     DATE,
                                  vMensaje   OUT VARCHAR2 ) IS
      nTurno NUMBER(1);
      --
BEGIN
         BEGIN
              SELECT MAX(Turno)
                INTO nTurno
                FROM CierreCajero
               WHERE CodigoAgencia = pAgencia
                 AND Fecha = pFecha
                 AND Moneda = pMoneda
                 AND UsrCodUsu = User;
         EXCEPTION WHEN OTHERS THEN
              Null; -- Se Coloca el Mensaje de Error
         END;
         --
         IF pOperacion = 'I' THEN
            BEGIN
                 UPDATE CierreCajero
                    SET Saldo = Nvl(Saldo,0) + Nvl(pImporte,0)
                  WHERE CodigoAgencia = pAgencia
                    AND Fecha = pFecha
                    AND UsrCodusu = User
                    AND Moneda = pMoneda
                    AND Turno = nTurno;
            EXCEPTION WHEN OTHERS THEN
                NULL; -- Se Coloca el Mensaje de Error
            END;
         ELSIF pOperacion = 'E' THEN
               BEGIN
                    UPDATE CierreCajero
                       SET Saldo = Nvl(Saldo,0) - Nvl(pImporte,0)
                     WHERE CodigoAgencia = pAgencia
                       AND Fecha = pFecha
                       AND UsrCodusu = User
                       AND Moneda = pMoneda
                       AND Turno = nTurno;
               EXCEPTION WHEN OTHERS THEN
                    NULL; -- Se Coloca el Mensaje de Error
               END;
         END IF;
         --
EXCEPTION WHEN OTHERS THEN
    vMensaje := 'P_ACT_ACTUALIZACAJERO : '||SQLerrm;
    RETURN;
END P_ACT_ACTUALIZACAJERO;
--
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
                                    ) IS
-- Log de cambios
-- 04.06.2019 - Juan Salazar C. - Optimizacion de codigo PROYECTO RECAUDACION
-- 19.08.2019 - Juan Salazar C. - Optimizacion de codigo PROYECTO RECAUDACION
--
  rInteres prestamo.IINTERE%TYPE := 0;
  rMora PRESTAMO.IMORA%TYPE := 0;
  nImporteIng Number;
  nContador   NUMBER;
  dFechaPivot date;
  --
  nCodigoAgencia CuentaCorriente.CodigoAgencia%Type;
  nSaldoPrestamo Prestamo.SaldoPrestamo%Type;
  dUltimoCapital Prestamo.UltimoCapital%Type;
  dUltimoInteres Prestamo.UltimoInteres%Type;
  dUltimoMora Prestamo.UltimaMora%Type;
  nItemPrestamo PrestamoPagos.NumeroItem%Type;
  dCapital PrestamoPagos.Amortizacion%Type;
  dInteres PrestamoPagos.Interes%Type;
  dInteresVencido PrestamoPagos.InteresNoCobrado%Type;
  dMora PrestamoPagos.InteresMoratorio%Type;
  dMoraVencido PrestamoPagos.IntMoranoCobrado%Type;
  nSaldoInteresNoCobrado PrestamoPagos.InteresNoCobrado%Type;
  nSaldoMoraNoCobrado PrestamoPagos.InteresNoCobrado%Type;
  nEstado Prestamo.Estado%Type;
  nMontoPrestamo Prestamo.MontoPrestamo%Type;
  nTipoProducto SYST902.TBLTIPOPRODUCTO%TYPE;
  dFechaDeposito Date := P_FechaDeposito;
BEGIN
     IF dFechaDeposito IS NULL THEN
        dFechaDeposito := P_FechaMovimiento;
     END IF;
     --
     nTipoProducto := PKG_SYST902.F_OBT_TBLTIPOPRODUCTO(PKG_PRESTAMODETALLE.F_OBT_TIPOSOLICITUD(P_PERIODOSOLICITUD, 
                                                                                                P_NUMEROSOLICITUD, 
                                                                                                PKG_PRESTAMODETALLE.F_OBT_MAX_NUMEROAMPLIACION(P_NUMEROSOLICITUD, P_PERIODOSOLICITUD)), 
                                                        PKG_PRESTAMODETALLE.F_OBT_TIPOPRESTAMO(P_PERIODOSOLICITUD, 
                                                                                               P_NUMEROSOLICITUD, PKG_PRESTAMODETALLE.F_OBT_MAX_NUMEROAMPLIACION(P_NUMEROSOLICITUD, P_PERIODOSOLICITUD)));
     --
     SELECT Nvl(P_CodigoAgencia, CodigoAgencia), 
            SaldoPrestamo, 
            UltimoCapital, 
            UltimoInteres, 
            UltimaMora, 
            MontoPrestamo
       INTO nCodigoAgencia, 
            nSaldoPrestamo, 
            dUltimoCapital, 
            dUltimoInteres, 
            dUltimoMora, 
            nMontoPrestamo
       FROM Prestamo
      WHERE PeriodoSolicitud = P_PeriodoSolicitud
        AND NumeroSolicitud = P_NumeroSolicitud;
     --
     BEGIN
          SELECT NVL(MAX(NumeroItem), 0) + 1 
            INTO nItemPrestamo
            FROM PrestamoPagos
           WHERE PeriodoSolicitud = P_PeriodoSolicitud
             AND NumeroSolicitud = P_NumeroSolicitud;
     EXCEPTION WHEN NO_DATA_FOUND THEN 
               nItemPrestamo := 1;
     END;
     --
     CRE07090(P_PeriodoSolicitud, P_NumeroSolicitud, dFechaDeposito, 2, 1, dCapital, dInteres, dInteresVencido, dMora, dMoraVencido);
     --
     IF P_Capital > 0 THEN
        dUltimoCapital := dFechaDeposito;
     END IF;
     --
     IF P_Interes + P_InteresNoCobrado > 0 THEN
        IF P_TipoMovimiento in (1, 3, 5) THEN
          nSaldoInteresNoCobrado := (dInteres + dInteresVencido) - (P_Interes + P_InteresNoCobrado);
        ELSE
            nSaldoInteresNoCobrado := (dInteres + dInteresVencido) + (P_Interes + P_InteresNoCobrado);
        END IF;
        IF nSaldoInteresNoCobrado < 0 THEN
           nSaldoInteresNoCobrado := 0;
        END IF;
        dUltimoInteres := dFechaDeposito;
     ELSE
         nSaldoInteresNoCobrado := dInteresVencido;
     END IF; 
     --
     IF P_Mora + P_MoraNoCobrado > 0 THEN
        IF P_TipoMovimiento in (1, 3, 5) THEN
           nSaldoMoraNoCobrado := (dMora + dMoraVencido) - (P_Mora + P_MoraNoCobrado);
        ELSE
            nSaldoMoraNoCobrado := (dMora + dMoraVencido) + (P_Mora + P_MoraNoCobrado);
        END IF;
        IF nSaldoMoraNoCobrado < 0 THEN
           nSaldoMoraNoCobrado := 0;
        END IF;
        dUltimoMora := dFechaDeposito;
     ELSE
         nSaldoMoraNoCobrado := dMoraVencido;
     END IF;
     --
     IF P_TipoMovimiento in (1, 3, 5) THEN
        nSaldoPrestamo := nSaldoPrestamo - P_Capital;
     ELSE
         nSaldoPrestamo := nSaldoPrestamo + P_Capital;
     END IF;
     --
     --GRABANDO EN PRESTAMOPAGOS
     INSERT INTO PrestamoPagos ( PeriodoSolicitud, NumeroSolicitud, NumeroItem, CodigoAgencia, Condicion, TipoMovimiento, FormaPago,
                                 FechaCancelacion, Amortizacion, Interes, InteresNoCobrado, Reajuste, InteresMoratorio,
                                 IntMoraNoCobrado, InteresAnterior, SaldoPrestamo, DiasMora, Estado, CodigoUsuario, FechaUsuario,
                                 Observaciones, TransferenciaGarantia, FechaDeposito, PORTES, SEGUROBIEN, CODIGOAGENCIACAJA, PERIODOCAJA, NUMEROCAJA)
                        VALUES ( P_PeriodoSolicitud, P_NumeroSolicitud, nItemPrestamo, nCodigoAgencia, P_Condicion, P_Tipomovimiento, P_FormaPago,
                                 P_FechaMovimiento, P_Capital, NVL(P_Interes,0), NVL(P_InteresNoCobrado,0), P_Desgravamen, NVL(P_Mora,0),
                                 NVL(P_MoraNoCobrado,0), 0, nSaldoPrestamo, 0, 1, User, Sysdate, P_Observaciones, NVL(P_TransferenciaGarantia,0),
                                 dFechaDeposito, P_Portes, P_SeguroBien, P_CODIGOAGENCIACAJA, P_PERIODOCAJA, P_NUMEROCAJA);
     --
     P_NumeroItem := nItemPrestamo;
     --
     IF nSaldoPrestamo = 0 AND nSaldoInteresNoCobrado = 0 AND nSaldoMoraNoCobrado = 0 THEN
        nEstado := 1;
     ELSE
         nEstado := 2;
     END IF;
     --Inicio Ricardo Mata 24/09/2016 : Actualizando la tabla DatosLinea, no se estaba considerando en el proceso del pago
     IF nvl(nTipoProducto,0) IN (1,8,10,13)  THEN
        UPDATE datoslinea
           SET saldolinea = nMontoPrestamo - nSaldoPrestamo,
               saldo = nSaldoPrestamo
         WHERE NumeroSolicitud = P_NumeroSolicitud
           AND PeriodoSolicitud = P_PeriodoSolicitud;
     END IF;
     --Fin Ricardo Mata 24/09/2016 : Actualizando la tabla DatosLinea, no se estaba considerando en el proceso del pago
     --
     --Inicio Ricardo Mata 23/09/2016 : Para los casos de reconocimiento a otra fecha para el STOCK, cuando se paga un poco de capital, se debe recalcular interes y mora
     IF TO_CHAR(dFechaDeposito,'DD/MM/YYYY') != TO_CHAR(ADM05040,'DD/MM/YYYY') THEN
        DBMS_OUTPUT.PUT_LINE('Entra prueba1');
        nImporteIng := P_Capital + P_Interes + P_Mora+ P_Desgravamen + P_Portes + P_SeguroBien;
        --
        IF P_TipoMovimiento in (1, 3, 5) THEN
            CRE07400(P_PERIODOSOLICITUD, P_NUMEROSOLICITUD, P_CAPITAL, P_INTERES, P_MORA, dFechaDeposito, rMora, rInteres);
        ELSE
            CRE07420(P_PERIODOSOLICITUD, P_NUMEROSOLICITUD,P_CAPITAL, P_INTERES,P_MORA, dFechaDeposito, rMORA, rInteres);
        END IF;
        --ACTUALIZANDO PRESTAMO
        UPDATE Prestamo
           SET SaldoPrestamo = nSaldoPrestamo, 
               UltimoCapital = dUltimoCapital, 
               UltimoInteres = dUltimoInteres, 
               UltimaMora = dUltimoMora,
               InteresNoCobrado = nvl(nSaldoInteresNoCobrado,0), 
               MoraNoCobrado = nvl(nSaldoMoraNoCobrado,0), 
               Estado = nEstado, 
               IINTERE = rInteres, 
               IMORA = rMora
         WHERE PeriodoSolicitud = P_PeriodoSolicitud
           AND NumeroSolicitud = P_NumeroSolicitud;
     ELSE
         --ACTUALIZANDO PRESTAMO
         IF P_TipoMovimiento in (1, 3, 5) THEN
            UPDATE Prestamo
               SET SaldoPrestamo = nSaldoPrestamo, 
                   UltimoCapital = dUltimoCapital, 
                   UltimoInteres = dUltimoInteres, 
                   UltimaMora = dUltimoMora,
                   InteresNoCobrado = nvl(nSaldoInteresNoCobrado,0), 
                   MoraNoCobrado = nvl(nSaldoMoraNoCobrado,0), 
                   Estado = nEstado, 
                   IINTERE = nvl(IINTERE,0) - nvl(P_Interes,0), 
                   IMORA = nvl(IMORA,0) - nvl(P_Mora,0)
             WHERE PeriodoSolicitud = P_PeriodoSolicitud
               AND NumeroSolicitud = P_NumeroSolicitud;
         ELSE
             UPDATE Prestamo
                SET SaldoPrestamo = nSaldoPrestamo, 
                    UltimoCapital = dUltimoCapital, 
                    UltimoInteres = dUltimoInteres, 
                    UltimaMora = dUltimoMora,
                    InteresNoCobrado = nvl(nSaldoInteresNoCobrado,0), 
                    MoraNoCobrado = nvl(nSaldoMoraNoCobrado,0), 
                    Estado = nEstado, 
                    IINTERE = nvl(IINTERE,0) - nvl(P_Interes,0), 
                    IMORA = nvl(IMORA,0) - nvl(P_Mora,0)
              WHERE PeriodoSolicitud = P_PeriodoSolicitud
                AND NumeroSolicitud = P_NumeroSolicitud;
         END IF;
     END IF;
     --Fin Ricardo Mata 23/09/2016 : Para los casos de reconocimiento a otra fecha para el STOCK, cuando se paga un poco de capital, se debe recalcular interes y mora
     --
     SELECT COUNT(*) 
       INTO nContador 
       FROM Prestamo
      WHERE PeriodoSolicitud = P_PeriodoSolicitud 
        AND NumeroSolicitud = P_NumeroSolicitud 
        AND nvl(iintere,0) < 0;
     --
     --IF nContador > 0 THEN
     IF TO_CHAR(P_FechaDeposito,'DD/MM/YYYY') = TO_CHAR(SYSDATE,'DD/MM/YYYY') THEN
        DBMS_OUTPUT.PUT_LINE('Entra Prestamo 1');
        BEGIN
             UPDATE Prestamo
                SET iintere = 0
              WHERE PeriodoSolicitud = P_PeriodoSolicitud 
                AND NumeroSolicitud = P_NumeroSolicitud;
             --
             SELECT MIN(FECHAVENCIMIENTO) 
               INTO dFechaPivot 
               FROM prestamocuotas 
              WHERE PeriodoSolicitud = P_PeriodoSolicitud 
                AND NumeroSolicitud = P_NumeroSolicitud 
                AND FECHAVENCIMIENTO > dFechaDeposito;
             --
             INSERT INTO PRESTAMOFECHAPIVOT( PERIODOSOLICITUD, 
                                             NUMEROSOLICITUD, 
                                             FECHAPIVOT, 
                                             TIPOPAGO
                                           )
                                    VALUES ( P_PeriodoSolicitud, 
                                             P_NumeroSolicitud, 
                                             dFechaPivot, 
                                             NULL);
        EXCEPTION WHEN DUP_VAL_ON_INDEX THEN
                        update PRESTAMOFECHAPIVOT
                           set FECHAPIVOT = dFechaPivot, TIPOPAGO = null
                         where PeriodoSolicitud = P_PeriodoSolicitud 
                           And NumeroSolicitud = P_NumeroSolicitud;
                  WHEN OTHERS THEN NULL; 
        END;
     ELSE
       SELECT COUNT(*) 
       INTO nContador 
       FROM PrestamoAnexo
       WHERE PeriodoSolicitud = P_PeriodoSolicitud 
        AND NumeroSolicitud = P_NumeroSolicitud 
        AND nvl(interes,0) < 0 
        AND FECHA = P_FechaDeposito+1;
        
        IF nContador > 0 THEN
        DBMS_OUTPUT.PUT_LINE('Entra PrestamoAnexo 2');
            BEGIN
                     UPDATE PrestamoAnexo
                        SET interes = 0
                      WHERE PeriodoSolicitud = P_PeriodoSolicitud 
                        AND NumeroSolicitud = P_NumeroSolicitud
                        AND FECHA >= P_FechaDeposito+1;
                     --
                      UPDATE Prestamo
                        SET iintere = 0
                      WHERE PeriodoSolicitud = P_PeriodoSolicitud 
                        AND NumeroSolicitud = P_NumeroSolicitud;
                     --
                     SELECT MIN(FECHAVENCIMIENTO) 
                       INTO dFechaPivot 
                       FROM prestamocuotas 
                      WHERE PeriodoSolicitud = P_PeriodoSolicitud 
                        AND NumeroSolicitud = P_NumeroSolicitud 
                        AND FECHAVENCIMIENTO > dFechaDeposito;
                     --
                     INSERT INTO PRESTAMOFECHAPIVOT( PERIODOSOLICITUD, 
                                                     NUMEROSOLICITUD, 
                                                     FECHAPIVOT, 
                                                     TIPOPAGO
                                                   )
                                            VALUES ( P_PeriodoSolicitud, 
                                                     P_NumeroSolicitud, 
                                                     dFechaPivot, 
                                                     NULL);
                    EXCEPTION WHEN DUP_VAL_ON_INDEX THEN
                                    update PRESTAMOFECHAPIVOT
                                       set FECHAPIVOT = dFechaPivot, TIPOPAGO = null
                                     where PeriodoSolicitud = P_PeriodoSolicitud 
                                       And NumeroSolicitud = P_NumeroSolicitud;
                              WHEN OTHERS THEN NULL; 
            END;
        END IF;

        
     END IF;
END CRE07132;
--
PROCEDURE P_OBT_VERIFICARDEBITOAUTO ( pPeriodoSolicitud NUMBER,
                                  pNumeroSolicitud   NUMBER,                                  
                                  vResultado OUT NUMBER ) IS
                                  
vExiste NUMBER;      
BEGIN         
        SELECT COUNT(*) INTO vExiste FROM maestrotransferencia
        WHERE tipooperacion=3
        AND periodosolicitud=pPeriodoSolicitud
        AND numerosolicitud=pNumeroSolicitud
        AND estado=1;         
                  --
         IF vExiste > 0 THEN
            vResultado :=1;
         ELSE
            vResultado :=2;
         END IF;
         --
EXCEPTION WHEN OTHERS THEN
    vResultado := 2;
    RETURN;
END P_OBT_VERIFICARDEBITOAUTO;
--
FUNCTION F_OBT_SALDOPORTES( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                            PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                            PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                          ) RETURN NUMBER IS
vSaldoInteres      NUMBER;
BEGIN
  SELECT SUM(PORTES)
    INTO vSaldoInteres
    FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodosolicitud, PINumerosolicitud, PIFechaPago )) 
   WHERE FECHAVENCIMIENTO <= PIFechaPago 
     AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
 
 RETURN vSaldoInteres;
END F_OBT_SALDOPORTES;
--
FUNCTION F_OBT_SALDOREAJUSTE( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                              PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                              PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                            ) RETURN NUMBER IS
vSaldoInteres      NUMBER;
BEGIN
  SELECT SUM(REAJUSTE)
    INTO vSaldoInteres
    FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodosolicitud, PINumerosolicitud, PIFechaPago )) 
   WHERE FECHAVENCIMIENTO <= PIFechaPago 
     AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
 
 RETURN vSaldoInteres;
END F_OBT_SALDOREAJUSTE;
--
FUNCTION F_OBT_SALDOSEGUROINTERES( PIPeriodosolicitud     IN prestamo.periodosolicitud%TYPE,
                                   PINumerosolicitud      IN prestamo.numerosolicitud%TYPE,
                                   PIFechaPago            IN recaudacionbanco.fechapago%TYPE
                                 ) RETURN NUMBER IS
vSaldoInteres      NUMBER;
BEGIN
  SELECT SUM(SEGUROINTERES)
    INTO vSaldoInteres
    FROM TABLE( CRE08070.DEUDACUOTASSIP(PIPeriodosolicitud, PINumerosolicitud, PIFechaPago )) 
   WHERE FECHAVENCIMIENTO <= PIFechaPago 
     AND (SALDOCAPITAL+SALDOINTERES+SALDOMORA)>0;
 
 RETURN vSaldoInteres;
END F_OBT_SALDOSEGUROINTERES;
--
END PKG_RECAUDACIONBANCO;
/