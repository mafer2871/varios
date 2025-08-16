create or replace PROCEDURE              P_GENERAR_LIQUIDACION (p_id_corte IN NUMBER, 
                                                   p_codigo      OUT NUMBER,
                                                   p_mensaje     OUT NUMBER)
IS
    vro_cortes                      jc_cortes%ROWTYPE;
    vro_usuario                     jc_usuarios%ROWTYPE;
    vro_cargo                       jc_cargos%ROWTYPE;
    vro_parametro                   jc_parametros%ROWTYPE;
    vro_acum_cu                     jc_acum_corte_usuarios%ROWTYPE;
    vro_acum_corte_usuarios         jc_acum_corte_usuarios%ROWTYPE;
    vro_acum_corte_usuarios_ant     jc_acum_corte_usuarios%ROWTYPE;
    vro_jc_corte_usuarios           jc_corte_usuarios%ROWTYPE;
    vro_jc_corte_anterior           jc_cortes%ROWTYPE;
    vty_fecha_inicial               jc_cortes.fecha_inicial%TYPE;
    vty_fecha_final                 jc_cortes.fecha_final%TYPE;
    vty_fecha_inicial_s             jc_cortes.fecha_inicial%TYPE;
    vty_fecha_final_s               jc_cortes.fecha_final%TYPE;
    vty_id_usuario                  jc_ejecucion_usuarios.id_usuario%TYPE;
    vty_id_obra                     jc_ejecucion_usuarios.id_obra%TYPE;
    v_porcentaje_prorata            jc_corte_usuarios.porcentaje_prorata%TYPE;
    vty_id_usuario_n                NUMBER;
    vty_id_usuario_ant              NUMBER := null;
    vnu_id_corte                    NUMBER;
    vnu_dias_festivos               NUMBER;
    vnu_diferencia_horas_a          NUMBER := 0;
    vnu_diferencia_horas1           NUMBER := 0;
    vnu_diferencia_horas2           NUMBER := 0;
    vnu_diferencia_horas3           NUMBER := 0;
    vnu_horas_corte                 NUMBER := 0;
    vnu_horas_corte_ori             NUMBER := 0;
    vnu_cantidad_dias_subsidio      NUMBER;
    vnu_sum_cantidad_horas          NUMBER;
    vnu_sum_cantidad_horas_extras   NUMBER;
    vnu_horas                       NUMBER := 0;
    vnu_horas_p                     NUMBER := 0;
    vnu_horas_s                     NUMBER := 0;
    vnu_valor_hora_s                NUMBER := 0;
    vnu_valor_hora_e                NUMBER := 0;
    vnu_valor_hora_subsidio         NUMBER;
    vnu_tiempo_autorizado           NUMBER;
    vnu_tiempo_pendiente            NUMBER;
    vty_horas_extra_festivo         NUMBER;
    vnu_valor_horas_festivo_ext     NUMBER;
    vbo_existe_corte_anterior       BOOLEAN;
    vbo_generar                     BOOLEAN := FALSE;
    vbo_existe                      BOOLEAN;
    vty_horas                       NUMBER;
    vty_horas_nocturnas             NUMBER;
    vty_horas_extras                NUMBER;
    vty_horas_extras_nocturna       NUMBER;
    vty_horas_festivos              NUMBER;
    vnu_valor_horas                 NUMBER;
    vnu_valor_horas_nocturnas       NUMBER;
    vnu_valor_horas_extra           NUMBER;
    vnu_valor_horas_extra_noct      NUMBER;
    vnu_valor_horas_festivo         NUMBER;
    vnu_calc_hora_festivo_noct      NUMBER;
    vnu_id_parametro                NUMBER;
    vnu_calc_hora_extra             NUMBER;
    vnu_calc_hora_nocturna          NUMBER;
    vnu_calc_hora_extra_nocturna    NUMBER;
    vnu_calc_hora_festivo           NUMBER;
    vnu_ref_subsidio_transporte     NUMBER;
    v_codigo                        NUMBER;
    v_compensa_extras_a             NUMBER; 
    v_compensa_extra_noche_a        NUMBER;
    v_compensa_festivo_a            NUMBER;
    v_compensa_total_a              NUMBER;
    v_compensa_extras               NUMBER; 
    v_compensa_extra_noche          NUMBER;
    v_compensa_festivo              NUMBER;
    v_compensa_total                NUMBER;
    v_cantidad_horas_nocturnas      NUMBER;
    v_subsidio_mens_transporte      NUMBER;
    vnu_subsidio                    NUMBER;
    vva_valor                       VARCHAR2(1000);
 
 
    vfe_fecha                       DATE;
    v_mensaje                       varchar2(1000);
    vbo_ingreso_quincena            BOOLEAN;
    
    CURSOR cur_corte IS
        SELECT *
          FROM jc_cortes
         WHERE (id = p_id_corte
           AND estado_corte = 'P');
 
    CURSOR cur_parametro IS
        SELECT * FROM jc_parametros
         WHERE id = vnu_id_parametro;
 
    CURSOR cur_subsidio_transporte IS
        SELECT valor_parametro
          FROM jc_parametros_fecha
         WHERE id_parametro = 7
           AND vro_cortes.fecha_final between fecha_inicial and fecha_final;
 
    CURSOR cur_usuarios_c IS
        SELECT DISTINCT id_usuario
          FROM jc_ejecucion_usuarios, jc_usuarios b
         WHERE id_usuario = b.ID
		   and b.estado_usuario = 'A' 
		   AND trunc(fecha) BETWEEN vty_fecha_inicial AND vty_fecha_final
           AND id_obra > 0
        ORDER BY id_usuario;
 
    CURSOR cur_usuarios IS
        SELECT DISTINCT id_obra, id_usuario
          FROM jc_ejecucion_usuarios, jc_usuarios b
         WHERE id_usuario = b.ID
		   and b.estado_usuario = 'A' 
		   AND trunc(fecha) BETWEEN vty_fecha_inicial AND vty_fecha_final
           AND id_obra > 0
        ORDER BY id_usuario, id_obra;
 
    CURSOR cur_ejecucion_semanal IS
        SELECT (NVL (SUM (e.cantidad_horas), 0) +  NVL (SUM (e.cantidad_horas_nocturnas), 0))          cantidad_horas
          FROM jc_ejecucion_usuarios e
         WHERE     e.id_usuario = vty_id_usuario
               AND exists (SELECT 1 FROM jc_usuarios u
                            WHERE e.id_usuario = u.id
                              AND u.estado_usuario = 'A')
               AND trunc(e.fecha) BETWEEN vty_fecha_inicial_s
                               AND vty_fecha_final_s
               AND TO_CHAR(e.fecha,'d') <> '1' 
               AND NOT EXISTS
                       (SELECT 1
                          FROM jc_dias_festivo f
                         WHERE e.fecha = f.fecha);
 
    CURSOR cur_ejecucion_normal IS
        SELECT NVL (SUM (e.cantidad_horas), 0)                                                              cantidad_horas,
               NVL (SUM (e.cantidad_horas_nocturnas), 0)                                                    cantidad_horas_nocturnas,
               NVL (SUM (e.cantidad_horas_extras), 0)                                                       cantidad_horas_extras,
               NVL (SUM (e.cantidad_horas_extra_nocturna), 0)                                               cantidad_horas_extras_noct,
               NVL (SUM (e.cantidad_horas * e.valor_hora), 0)                                               valor_horas,
               NVL (SUM (e.cantidad_horas_nocturnas * e.valor_hora * vnu_calc_hora_nocturna), 0)            valor_horas_nocturnas,
               NVL (SUM (e.cantidad_horas_extras * e.valor_hora * vnu_calc_hora_extra), 0)                  valor_horas_extras,
               NVL (SUM (e.cantidad_horas_extra_nocturna * e.valor_hora * vnu_calc_hora_extra_nocturna), 0) valor_horas_extras_noct
          FROM jc_ejecucion_usuarios e
         WHERE     e.id_obra = vty_id_obra
               AND e.id_usuario = vty_id_usuario
               AND exists (SELECT 1 FROM jc_usuarios u
                            WHERE e.id_usuario = u.id
                              AND u.estado_usuario = 'A')
               AND trunc(e.fecha) BETWEEN vro_cortes.fecha_inicial
                               AND vro_cortes.fecha_final
               AND TO_CHAR(e.fecha,'d') <> '1' 
               AND NOT EXISTS
                       (SELECT 1
                          FROM jc_dias_festivo f
                         WHERE e.fecha = f.fecha);
 
    CURSOR cur_ejecucion_normal_dia IS
        SELECT NVL (SUM (e.cantidad_horas), 0)           cantidad_horas
          FROM jc_ejecucion_usuarios e
         WHERE     e.id_obra = vty_id_obra
               AND e.id_usuario = vty_id_usuario
               AND exists (SELECT 1 FROM jc_usuarios u
                            WHERE e.id_usuario = u.id
                              AND u.estado_usuario = 'A')
               AND e.fecha = vfe_fecha;
 
    CURSOR cur_ejecucion_festivo IS
        SELECT NVL (SUM (e.cantidad_horas), 0)     cantidad_horas, 
               NVL (SUM (e.cantidad_horas * e.valor_hora * vnu_calc_hora_festivo), 0)     valor_horas_festivos,
               NVL (SUM (e.cantidad_horas_nocturnas), 0) cantidad_horas_nocturnas,
               NVL (SUM (e.cantidad_horas_nocturnas * e.valor_hora * vnu_calc_hora_festivo_noct), 0)   valor_horas_nocturnas
          FROM jc_ejecucion_usuarios e
         WHERE     e.id_obra = vty_id_obra
               AND e.id_usuario = vty_id_usuario 
               AND exists (SELECT 1 FROM jc_usuarios u
                            WHERE e.id_usuario = u.id
                              AND u.estado_usuario = 'A')
               AND trunc(e.fecha) BETWEEN vro_cortes.fecha_inicial
                               AND vro_cortes.fecha_final
               AND ((EXISTS
                       (SELECT 1
                          FROM jc_dias_festivo f
                         WHERE e.fecha = f.fecha)) OR
                      TO_CHAR(e.fecha,'d') = '1');
 
    CURSOR cur_dias_ejecucion IS
        SELECT distinct fecha
          FROM jc_ejecucion_usuarios e
         WHERE     e.id_obra = vty_id_obra
               AND e.id_usuario = vty_id_usuario
               AND exists (SELECT 1 FROM jc_usuarios u
                            WHERE e.id_usuario = u.id
                              AND u.estado_usuario = 'A')
               AND trunc(e.fecha) BETWEEN vro_cortes.fecha_inicial
                               AND vro_cortes.fecha_final;
 
    CURSOR cur_dias_ejecucion_usu IS
        SELECT distinct fecha
          FROM jc_ejecucion_usuarios e
         WHERE     e.id_usuario = vty_id_usuario
               AND e.cantidad_horas > 0
               AND exists (SELECT 1 FROM jc_usuarios u
                            WHERE e.id_usuario = u.id
                              AND u.estado_usuario = 'A')
               AND trunc(e.fecha) BETWEEN vro_cortes.fecha_inicial
                               AND vro_cortes.fecha_final;
 
    CURSOR cur_par_subs_transporte IS
        SELECT valor_parametro
          FROM jc_parametros_fecha
         WHERE id_parametro = 7
           AND fecha_inicial <= vfe_fecha and (fecha_final is null or fecha_final >= vfe_fecha);
 
    CURSOR cur_sum_usuario IS
        SELECT SUM(cantidad_horas), SUM(cantidad_horas_extras), SUM(cantidad_horas_nocturnas)
          FROM jc_ejecucion_usuarios
         WHERE id_usuario = vty_id_usuario
           AND trunc(fecha) BETWEEN vro_cortes.fecha_inicial
                         AND vro_cortes.fecha_final;
 
    CURSOR cur_sum_tiempo_autoriza IS
        SELECT SUM(tiempo_autorizado), SUM(tiempo_pendiente)
          FROM jc_ejecucion_usuarios
         WHERE id_usuario = vty_id_usuario
           AND trunc(fecha) BETWEEN vro_cortes.fecha_inicial
                         AND vro_cortes.fecha_final;
 
    CURSOR cur_sum_tiempo_compensado IS
        SELECT SUM(cantidad_normal)     compensa_extras, 
               SUM(cantidad_nocturna)   compensa_extra_noche,
               SUM(cantidad_festivo)    compensa_festivo,
               SUM(total_compensar)      compensa_total
          FROM jc_autoriza_descuento
         WHERE id_usuario = vty_id_usuario
           AND id_corte = p_id_corte;
 
    CURSOR cur_usuario_nov IS
        SELECT DISTINCT id_usuario
          FROM jc_acum_corte_usuarios
         WHERE id_corte = p_id_corte
           AND id_usuario = vty_id_usuario;
 
    CURSOR cur_usuarios_sin_obra IS
        SELECT distinct id_usuario
          FROM jc_ejecucion_usuarios cu
         WHERE cu.fecha BETWEEN vty_fecha_inicial AND vty_fecha_final
           AND NOT EXISTS (
                SELECT  jc.id_usuario
                  FROM jc_ejecucion_usuarios jc, jc_usuarios b
                 WHERE jc.id_usuario = b.ID
                   AND jc.id_usuario = cu.id_usuario
        		   and b.estado_usuario = 'A' 
        		   AND trunc(jc.fecha) BETWEEN vty_fecha_inicial AND vty_fecha_final
                   AND jc.id_obra > 0);
 
    CURSOR cur_acum_corte_usuario IS
        SELECT *
          FROM jc_acum_corte_usuarios
         WHERE id_corte = vnu_id_corte
           AND id_usuario = vty_id_usuario;
 
    CURSOR cur_corte_anterior IS
        SELECT * FROM (SELECT *
                                FROM jc_cortes
                               WHERE estado_corte = 'E'
                               ORDER BY fecha_inicial desc)
          WHERE rownum = 1;
 
     CURSOR cur_autoriza_descuento IS
        SELECT SUM(TOTAL_COMPENSAR) FROM jc_autoriza_descuento
         WHERE id_corte = vnu_id_corte
           AND id_usuario = vty_id_usuario;
 
    CURSOR cur_sum_usuario_corte IS
        SELECT SUM(cantidad_horas), SUM(cantidad_horas_nocturnas)
          FROM jc_corte_usuarios
         WHERE id_usuario = vty_id_usuario
           AND id_corte = p_id_corte;
 
    CURSOR cur_sum_usuario_obra_corte IS
        SELECT id_obra, SUM(cantidad_horas) cantidad_horas, SUM(cantidad_horas_nocturnas) cantidad_horas_nocturnas
          FROM jc_corte_usuarios
         WHERE id_usuario = vty_id_usuario
           AND id_corte = p_id_corte
          group by id_obra;
 
BEGIN
    p_codigo := 0;
    -- Traer el valor de total de horas para el periodo.
    IF pkgjc_parametros_dao.f_existe(1, vro_parametro) = TRUE THEN
        vnu_horas_corte := vro_parametro.valor_parametro;
        vnu_horas_corte_ori     := vnu_horas_corte;
    END IF;
    -- Traer el valor de las horas maximas normales de ley.
    IF pkgjc_parametros_dao.f_existe(9, vro_parametro) = TRUE THEN
        vnu_horas_p := vro_parametro.valor_parametro;
    ELSE
        vnu_horas_p := 46;
    END IF;
 
    OPEN cur_corte;
    FETCH cur_corte INTO vro_cortes;
    IF cur_corte%FOUND
    THEN
        vnu_id_corte := vro_cortes.id;
        vnu_id_parametro := 4; --Valor hora extra
        if f_valor_parametro(vnu_id_parametro, vro_cortes.fecha_inicial, vva_valor) = FALSE then
            p_codigo := 1;
            p_mensaje := 'No encontró el parametro 4 para calcular el valor de hora extra';
        ELSE
            vnu_calc_hora_extra  := to_number(vva_valor);
        END IF;
 
        vnu_id_parametro := 5; -- Múltiplo del valor hora para calcular el valor de hora extra nocturna
        if f_valor_parametro(vnu_id_parametro, vro_cortes.fecha_inicial, vva_valor) = FALSE then
            p_codigo := 1;
            p_mensaje := 'No encontró el parametro 4 para calcular el valor de hora extra';
        ELSE
            vnu_calc_hora_extra_nocturna  := to_number(vva_valor);
        END IF;
 
        vnu_id_parametro := 10; -- Múltiplo del valor hora para calcular el valor de hora nocturna
        if f_valor_parametro(vnu_id_parametro, vro_cortes.fecha_inicial, vva_valor) = FALSE then
            p_codigo := 1;
            p_mensaje := 'No encontró el parametro 4 para calcular el valor de hora extra';
        ELSE
            vnu_calc_hora_nocturna  := to_number(vva_valor);
        END IF;
 
        vnu_id_parametro := 6; --	Múltiplo del valor hora para calcular el valor de hora festivo
        if f_valor_parametro(vnu_id_parametro, vro_cortes.fecha_inicial, vva_valor) = FALSE then
            p_codigo := 1;
            p_mensaje := 'No encontró el parametro 4 para calcular el valor de hora extra';
        ELSE
            vnu_calc_hora_festivo  := to_number(vva_valor);
        END IF;
 
        vnu_id_parametro := 12; --	Múltiplo del valor del recargo nocturno dominical o festivo
        if f_valor_parametro(vnu_id_parametro, vro_cortes.fecha_inicial, vva_valor) = FALSE then
            p_codigo := 1;
            p_mensaje := 'No encontró el parametro 12 para calcular el valor de hora extra';
        ELSE
            vnu_calc_hora_festivo_noct  := to_number(vva_valor);
        END IF;
--dbms_output.put_line('1.Codigo de proceso: '||p_codigo);
        IF p_codigo <> 0 THEN
            null;
        ELSE
            p_act_hora_ejecucion(vro_cortes.id, v_codigo, v_mensaje);
            vty_fecha_inicial := vro_cortes.fecha_inicial;
            vty_fecha_final := vro_cortes.fecha_final;
 
            IF vro_cortes.estado_corte = 'E'
            THEN
                vbo_generar := FALSE;
            ELSE
                vbo_generar := TRUE;
                DELETE FROM jc_corte_usuarios
                    WHERE id_corte = p_id_corte;
--dbms_output.put_line('Borrando: '||p_id_corte);                    
                DELETE FROM jc_acum_corte_usuarios
                     WHERE id_corte = p_id_corte;
                DELETE FROM jc_archivo_nomina
                    WHERE id_periodo = p_id_corte;
            END IF;
 
            IF vbo_generar = TRUE
            THEN
                FOR u IN cur_usuarios
                LOOP
                    vty_id_obra := u.id_obra;
                    vty_id_usuario := u.id_usuario;
 
                    IF vty_id_usuario_ant is null or vty_id_usuario_ant <> vty_id_usuario THEN
                        OPEN cur_sum_tiempo_compensado;
                        FETCH cur_sum_tiempo_compensado INTO v_compensa_extras, v_compensa_extra_noche, v_compensa_festivo, v_compensa_total;
                        CLOSE cur_sum_tiempo_compensado;
                        v_compensa_extras_a       := 0;
                        v_compensa_extra_noche_a  := 0;
                        v_compensa_festivo_a      := 0;
                    ELSIF vty_id_usuario_ant = vty_id_usuario THEN
                        v_compensa_extras       := v_compensa_extras_a;
                        v_compensa_extra_noche  := v_compensa_extra_noche_a;
                        v_compensa_festivo      := v_compensa_festivo_a;
                    END IF;
 
                    OPEN cur_sum_tiempo_autoriza;
                    FETCH cur_sum_tiempo_autoriza INTO vnu_tiempo_autorizado, vnu_tiempo_pendiente;
                    CLOSE cur_sum_tiempo_autoriza;
 
                    vty_horas := 0;
                    vty_horas_extras := 0;
                    vty_horas_festivos := 0;
                    vty_horas_extras_nocturna := 0;
                    --pkgjc_corte_usuarios_dao.p_valores_defecto(vro_jc_corte_usuarios);
                    vro_jc_corte_usuarios.id                            := null; 
                    vro_jc_corte_usuarios.id_corte                      := null;
                    vro_jc_corte_usuarios.id_obra                       := null;
                    vro_jc_corte_usuarios.id_usuario                    := null;
                    vro_jc_corte_usuarios.cantidad_horas                := null;
                    vro_jc_corte_usuarios.cantidad_horas_nocturnas      := null;
                    vro_jc_corte_usuarios.cantidad_horas_extras         := null;
                    vro_jc_corte_usuarios.cantidad_horas_festivos       := null;
                    vro_jc_corte_usuarios.cantidad_horas_extras_noche   := null;
                    vro_jc_corte_usuarios.subsidio_transporte           := null;
                    vro_jc_corte_usuarios.valor_horas                   := null;
                    vro_jc_corte_usuarios.valor_horas_nocturnas         := null;
                    vro_jc_corte_usuarios.valor_horas_extra             := null;
                    vro_jc_corte_usuarios.valor_horas_festivo           := null;        
                    vro_jc_corte_usuarios.valor_horas_extras_noche      := null;
                    vro_jc_corte_usuarios.porcentaje_prorata            := null;
                    vro_jc_corte_usuarios.tiempo_compensar              := null;
                    vro_jc_corte_usuarios.cantidad_horas_noct_fest      := null;      
                    vro_jc_corte_usuarios.valor_horas_noct_fest         := null;
                    vro_jc_corte_usuarios.dias_subsidio_transporte      := null;
 
                    OPEN cur_ejecucion_normal;
                    FETCH cur_ejecucion_normal INTO vty_horas,          vty_horas_nocturnas,       vty_horas_extras,       vty_horas_extras_nocturna,
                                                    vnu_valor_horas,    vnu_valor_horas_nocturnas, vnu_valor_horas_extra,  vnu_valor_horas_extra_noct;
                    CLOSE cur_ejecucion_normal;
 
                    IF vty_horas_extras >= v_compensa_extras THEN
                        vty_horas_extras := vty_horas_extras - v_compensa_extras;
                        v_compensa_extras_a := 0;
                    ELSE
                        vty_horas_extras := 0;
                        v_compensa_extras_a := v_compensa_extras - vty_horas_extras;
                    END IF;
                    IF vty_horas_extras_nocturna >= v_compensa_extra_noche THEN
                        vty_horas_extras_nocturna := vty_horas_extras_nocturna - v_compensa_extra_noche;
                        v_compensa_extra_noche_a := 0;
                    ELSE
                        vty_horas_extras_nocturna := 0;
                        v_compensa_extra_noche_a := v_compensa_extra_noche - vty_horas_extras;
                    END IF;
                    vro_cargo.administrativo := null;
                    IF pkgjc_usuarios_dao.f_existe(u.id_usuario, vro_usuario) = TRUE THEN
                        IF pkgjc_cargos_dao.f_existe(vro_usuario.id_cargo, vro_cargo) = TRUE THEN
                            IF vro_cargo.administrativo = 'S' THEN
                                vty_horas_extras := 0;
                                vnu_valor_horas_extra := 0;
                                vty_horas_extras_nocturna := 0;
                                vnu_valor_horas_extra_noct := 0;
                            END IF;
                        END IF;
                    END IF;
                    
                    OPEN cur_ejecucion_festivo;
                    FETCH cur_ejecucion_festivo INTO vty_horas_festivos, vnu_valor_horas_festivo, vty_horas_extra_festivo, vnu_valor_horas_festivo_ext;
                    CLOSE cur_ejecucion_festivo;
                    IF vro_cargo.administrativo = 'S' THEN
                        vty_horas_extra_festivo := 0;
                        vnu_valor_horas_festivo_ext := 0;
                    END IF;
 
                    IF vty_horas_festivos >= NVL(v_compensa_festivo,0) THEN
                        vty_horas_festivos:= vty_horas_festivos - NVL(v_compensa_festivo,0);
                        v_compensa_festivo_a := 0;
                    ELSE
                        vty_horas_festivos := 0;
                        v_compensa_festivo_a := NVL(v_compensa_festivo,0) - vty_horas_festivos;
                    END IF;
 
                    -- Buscar por semana cuanto suman las horas, si se pasa de 48 horas, se deben pagar como extras, si es administrativo se borran las extras.
                    IF vty_id_usuario_ant is null or vty_id_usuario_ant <> vty_id_usuario THEN
                        vty_id_usuario_ant := vty_id_usuario;
                        IF vty_horas > 0 THEN
                            vnu_valor_hora_s    := vnu_valor_horas / vty_horas;
                        ELSE
                            vnu_valor_hora_s    := 0;
                        END IF;
                        vnu_valor_hora_e    := vnu_valor_horas * vnu_calc_hora_extra;
 
                         IF  TRIM(TO_CHAR((vty_fecha_inicial), 'Day', 'nls_date_language = SPANISH')) = 'Lunes' THEN
                            vty_fecha_inicial_s     := vty_fecha_inicial;
                            vty_fecha_final_s       := vty_fecha_inicial + 5;
                        ELSIF TRIM(TO_CHAR((vty_fecha_inicial), 'Day', 'nls_date_language = SPANISH')) = 'Domingo' THEN
                            vty_fecha_inicial_s     := vty_fecha_inicial + 1;
                            vty_fecha_final_s       := vty_fecha_inicial_s + 5;
                       ELSE                        
                            vty_fecha_inicial_s     := next_day((vty_fecha_inicial-7),'LUNES');
                            vty_fecha_final_s       := vty_fecha_inicial_s + 5;
                        END IF;
                        vnu_diferencia_horas1 := 0;
                        vnu_diferencia_horas2 := 0;
                        vnu_diferencia_horas_a := 0;
 
                        OPEN cur_ejecucion_semanal;
                        FETCH cur_ejecucion_semanal INTO vnu_horas_s;
                        CLOSE cur_ejecucion_semanal;
                        IF vnu_horas_p < vnu_horas_s THEN
                            vnu_diferencia_horas1 := vnu_horas_s - vnu_horas_p;
                            -- la diferencia se descuenta de las horas normales y se suman en horas extras si no es administrativo
                            IF NVL(vro_cargo.administrativo,'') <> 'S' THEN
                                IF vnu_diferencia_horas1 <= vty_horas THEN
                                    vty_horas               := vty_horas - vnu_diferencia_horas1;
                                    vnu_valor_horas         := vnu_valor_horas - (vnu_diferencia_horas1 * (vnu_valor_hora_s));
                                    vty_horas_extras        := NVL(vty_horas_extras,0) + vnu_diferencia_horas1;
                                    vnu_valor_horas_extra   := vnu_valor_horas_extra + (vnu_diferencia_horas1 * (vnu_valor_hora_e));
                                ELSE
                                    vty_horas_extras        := NVL(vty_horas_extras,0) + vty_horas;
                                    vnu_valor_horas_extra   := vnu_valor_horas_extra + (vty_horas * (vnu_valor_hora_e));
                                    vty_horas               := 0;
                                    vnu_valor_horas         := 0;
                                    vnu_diferencia_horas_a  := (vnu_diferencia_horas1 - vty_horas);
                                END IF;
                            END IF;
                        END IF;
 
                        -- Se procesa la segunda semana q siempre es completa
                        vty_fecha_inicial_s             := next_day(vty_fecha_final_s,'LUNES');
                        vty_fecha_final_s               := vty_fecha_inicial_s + 5;
                       -- IF vty_fecha_final <= vty_fecha_final_s THEN
                            vnu_diferencia_horas2 := 0;
                            OPEN cur_ejecucion_semanal;
                            FETCH cur_ejecucion_semanal INTO vnu_horas_s;
                            CLOSE cur_ejecucion_semanal;
                            IF vnu_horas_p < vnu_horas_s THEN
                                vnu_diferencia_horas2 := vnu_horas_s - vnu_horas_p;
                                -- la diferencia se descuenta de las horas normales y se suman en horas extras si no es administrativo.
                                IF NVL(vro_cargo.administrativo,'') <> 'S' THEN
                                    /*vty_horas_extras        := NVL(vty_horas_extras,0) + vnu_diferencia_horas2;
                                    vnu_valor_horas_extra   := vnu_valor_horas_extra + (vnu_diferencia_horas2 * (vnu_valor_hora_e));
                                    vty_horas               := vty_horas - vnu_diferencia_horas2;
                                    vnu_valor_horas         := vnu_valor_horas - (vnu_diferencia_horas2 * (vnu_valor_hora_s));*/
                                    IF vty_horas > 0 THEN
                                        IF vnu_diferencia_horas2 <= vty_horas THEN
                                            vty_horas               := vty_horas - vnu_diferencia_horas2;
                                            vnu_valor_horas         := vnu_valor_horas - (vnu_diferencia_horas2 * (vnu_valor_hora_s));
                                            vty_horas_extras        := NVL(vty_horas_extras,0) + vnu_diferencia_horas2;
                                            vnu_valor_horas_extra   := vnu_valor_horas_extra + (vnu_diferencia_horas2 * (vnu_valor_hora_e));
                                        ELSE
                                            vty_horas_extras        := NVL(vty_horas_extras,0) + vty_horas;
                                            vnu_valor_horas_extra   := vnu_valor_horas_extra + (vty_horas * (vnu_valor_hora_e));
                                            vty_horas               := 0;
                                            vnu_valor_horas         := 0;
                                            vnu_diferencia_horas_a  := (vnu_diferencia_horas2 - vty_horas);
                                        END IF;
                                    ELSE
                                        vnu_diferencia_horas_a := vnu_diferencia_horas2;
                                    END IF;
                                END IF;
                            END IF;
                        --END IF;
 
                        -- Se busca la tercera semana si es completa
                        vty_fecha_inicial_s         := next_day(vty_fecha_final_s,'LUNES');
                        vty_fecha_final_s           := vty_fecha_inicial_s + 5;
                        IF vty_fecha_inicial_s < vty_fecha_final THEN
                            IF vty_fecha_final_s > vty_fecha_final THEN
                                vty_fecha_final_s := vty_fecha_final;
                            END IF;
                        --IF vty_fecha_final_s <= vty_fecha_final THEN
                            vnu_diferencia_horas3 := 0;
                            OPEN cur_ejecucion_semanal;
                            FETCH cur_ejecucion_semanal INTO vnu_horas_s;
                            CLOSE cur_ejecucion_semanal;
                            IF vnu_horas_p < vnu_horas_s THEN
                                vnu_diferencia_horas3 := vnu_horas_s - vnu_horas_p;
                                -- la diferencia se descuenta de las horas normales y se suman en horas extras.
                                IF NVL(vro_cargo.administrativo,'') <> 'S' THEN
                                    /*vty_horas_extras        := NVL(vty_horas_extras,0) + vnu_diferencia_horas3;
                                    vnu_valor_horas_extra   := vnu_valor_horas_extra + (vnu_diferencia_horas3 * (vnu_valor_hora_e));
                                    vty_horas               := vty_horas - vnu_diferencia_horas3;
                                    vnu_valor_horas         := vnu_valor_horas - (vnu_diferencia_horas3 * (vnu_valor_hora_s));*/
                                    IF vty_horas > 0 THEN
                                        IF vnu_diferencia_horas3 <= vty_horas THEN
                                            vty_horas               := vty_horas - vnu_diferencia_horas3;
                                            vnu_valor_horas         := vnu_valor_horas - (vnu_diferencia_horas3 * (vnu_valor_hora_s));
                                            vty_horas_extras        := NVL(vty_horas_extras,0) + vnu_diferencia_horas3;
                                            vnu_valor_horas_extra   := vnu_valor_horas_extra + (vnu_diferencia_horas3 * (vnu_valor_hora_e));
                                        ELSE
                                            vty_horas_extras        := NVL(vty_horas_extras,0) + vty_horas;
                                            vnu_valor_horas_extra   := vnu_valor_horas_extra + (vty_horas * (vnu_valor_hora_e));
                                            vty_horas               := 0;
                                            vnu_valor_horas         := 0;
                                            vnu_diferencia_horas_a  := (vnu_diferencia_horas3 - vty_horas);
                                        END IF;
                                    ELSE
                                        vnu_diferencia_horas_a := vnu_diferencia_horas3;
                                    END IF;
 
                                END IF;
                            END IF;
                        END IF;
                    ELSE
                        IF vty_id_usuario_ant = vty_id_usuario and vnu_diferencia_horas_a > 0 THEN
                            IF vty_horas > 0 THEN
                                IF vnu_diferencia_horas_a <= vty_horas THEN
                                    vty_horas               := vty_horas - vnu_diferencia_horas_a;
                                    vnu_valor_horas         := vnu_valor_horas - (vnu_diferencia_horas_a * (vnu_valor_hora_s));
                                    vty_horas_extras        := NVL(vty_horas_extras,0) + vnu_diferencia_horas_a;
                                    vnu_valor_horas_extra   := vnu_valor_horas_extra + (vnu_diferencia_horas_a * (vnu_valor_hora_e));
                                ELSE
                                    vty_horas_extras        := NVL(vty_horas_extras,0) + vty_horas;
                                    vnu_valor_horas_extra   := vnu_valor_horas_extra + (vty_horas * (vnu_valor_hora_e));
                                    vty_horas               := 0;
                                    vnu_valor_horas         := 0;
                                    vnu_diferencia_horas_a  := (vnu_diferencia_horas_a - vty_horas);
                                END IF;
                            ELSE
                                vnu_diferencia_horas_a := vnu_diferencia_horas3;
                            END IF;
                        END IF;                    --
                    END IF;
                    vro_jc_corte_usuarios.id                           := jc_corte_usuarios_seq.NEXTVAL;
                    vro_jc_corte_usuarios.id_obra                      := vty_id_obra;
                    vro_jc_corte_usuarios.id_usuario                   := vty_id_usuario;
                    vro_jc_corte_usuarios.cantidad_horas               := vty_horas;
                    vro_jc_corte_usuarios.cantidad_horas_nocturnas     := vty_horas_nocturnas;
                    vro_jc_corte_usuarios.cantidad_horas_extras        := vty_horas_extras;
                    vro_jc_corte_usuarios.cantidad_horas_festivos      := vty_horas_festivos;
                    vro_jc_corte_usuarios.cantidad_horas_extras_noche  := vty_horas_extras_nocturna;
                    vro_jc_corte_usuarios.cantidad_horas_noct_fest     := vty_horas_extra_festivo;
                    vro_jc_corte_usuarios.valor_horas                  := vnu_valor_horas;
                    vro_jc_corte_usuarios.valor_horas_nocturnas        := vnu_valor_horas_nocturnas;
                    vro_jc_corte_usuarios.valor_horas_extra            := vnu_valor_horas_extra;
                    vro_jc_corte_usuarios.valor_horas_festivo          := vnu_valor_horas_festivo;
                    vro_jc_corte_usuarios.valor_horas_extras_noche     := vnu_valor_horas_extra_noct;
                    vro_jc_corte_usuarios.valor_horas_noct_fest        := vnu_valor_horas_festivo_ext;
                    vro_jc_corte_usuarios.subsidio_transporte          := vnu_ref_subsidio_transporte;
                    
                    INSERT INTO jc_corte_usuarios (id,
                                                id_corte,
                                                id_obra,
                                                id_usuario,
                                                cantidad_horas,
                                                cantidad_horas_nocturnas,
                                                cantidad_horas_extras,
                                                cantidad_horas_festivos,
                                                cantidad_horas_extras_noche,
                                                cantidad_horas_noct_fest,
                                                valor_horas,
                                                valor_horas_nocturnas,
                                                valor_horas_extra,
                                                valor_horas_festivo,
                                                valor_horas_extras_noche,
                                                valor_horas_noct_fest)
                        VALUES (vro_jc_corte_usuarios.id,
                                p_id_corte,
                                vro_jc_corte_usuarios.id_obra,
                                vro_jc_corte_usuarios.id_usuario,
                                vro_jc_corte_usuarios.cantidad_horas,
                                vro_jc_corte_usuarios.cantidad_horas_nocturnas,
                                vro_jc_corte_usuarios.cantidad_horas_extras,
                                vro_jc_corte_usuarios.cantidad_horas_festivos,
                                vro_jc_corte_usuarios.cantidad_horas_extras_noche,
                                vro_jc_corte_usuarios.cantidad_horas_noct_fest,
                                vro_jc_corte_usuarios.valor_horas,
                                vro_jc_corte_usuarios.valor_horas_nocturnas,
                                vro_jc_corte_usuarios.valor_horas_extra,
                                vro_jc_corte_usuarios.valor_horas_festivo,
                                vro_jc_corte_usuarios.valor_horas_extras_noche,
                                vro_jc_corte_usuarios.valor_horas_noct_fest
                                );
 
                        
                END LOOP;
 
                vnu_dias_festivos := 0;
                FOR dia IN 1 .. ((trunc(vro_cortes.fecha_final) - trunc(vro_cortes.fecha_inicial))+1) LOOP
                    if f_dia_festivo ((vro_cortes.fecha_inicial) + (dia - 1)) = 1 THEN
                        vnu_dias_festivos := vnu_dias_festivos + 1;
                    end if;
                END LOOP;
 
                -- Traer el valor del subsidio de transporte.
                open cur_subsidio_transporte;
                fetch cur_subsidio_transporte into v_subsidio_mens_transporte;
                close cur_subsidio_transporte;
 
                FOR uc IN cur_usuarios_c LOOP
                    vnu_horas_corte := vnu_horas_corte_ori;
                    vty_id_usuario := uc.id_usuario;
                    IF pkgjc_usuarios_dao.f_existe(vty_id_usuario, vro_usuario) THEN
                        IF NVL(vro_usuario.fecha_ingreso,vro_cortes.fecha_inicial) > vro_cortes.fecha_inicial THEN
                            --Ingreso despues del corte, se calcula las horas del corte.
                            vnu_horas_corte := ((vro_cortes.fecha_final - vro_usuario.fecha_ingreso) * vnu_horas_corte_ori) / 15;  --original 120
                        END IF;
                    END IF;
                    OPEN cur_sum_tiempo_autoriza;
                    FETCH cur_sum_tiempo_autoriza INTO vnu_tiempo_autorizado, vnu_tiempo_pendiente;
                    CLOSE cur_sum_tiempo_autoriza;
          --IF vty_id_usuario = 259THEN
   -- dbms_output.put_line('ID: '||vty_id_usuario||' Tiempo autorizado: '||vnu_tiempo_autorizado|| ' ' ||' Tiempo Pendiente: '||vnu_tiempo_pendiente);
--END IF;   
                    v_porcentaje_prorata := 1;
                    --Calcular el valor de prorata. 
                    IF (vnu_horas_corte - NVL(vnu_tiempo_autorizado,0) - NVL(vnu_tiempo_pendiente,0)) > 0 THEN
                        vro_jc_corte_usuarios.porcentaje_prorata := 1;
                        -- Traer valores del corte para el usuario.
                        OPEN cur_sum_usuario_corte;
                        FETCH cur_sum_usuario_corte INTO vnu_sum_cantidad_horas, v_cantidad_horas_nocturnas;
                        CLOSE cur_sum_usuario_corte;
                        IF (vnu_horas_corte - NVL(vnu_tiempo_autorizado,0) - NVL(vnu_tiempo_pendiente,0)) > (NVL(vnu_sum_cantidad_horas,0) + NVL(v_cantidad_horas_nocturnas,0)) THEN
                            v_porcentaje_prorata := 0;
                            if (NVL(vnu_sum_cantidad_horas,0) + NVL(v_cantidad_horas_nocturnas,0)) != 0 then
                                v_porcentaje_prorata := (vnu_horas_corte - NVL(vnu_tiempo_autorizado,0) - NVL(vnu_tiempo_pendiente,0))  / 
                                                                        (NVL(vnu_sum_cantidad_horas,0) + NVL(v_cantidad_horas_nocturnas,0));
                            else
                                insert into  log_mensajes  values (sysdate, 'Usuario: '||vty_id_usuario);
                            end if;
                        END IF;
                    END IF;
 
                    vnu_ref_subsidio_transporte := 0;
                    vnu_cantidad_dias_subsidio := 0;
                    FOR de IN cur_dias_ejecucion_usu LOOP 
                        vfe_fecha := de.fecha; 
                        vnu_valor_hora_subsidio := F_VALOR_HORA_SUBSIDIO (vfe_fecha);
                        -- Se suman los días que tiene derecho al subsidio.
                        IF F_VALOR_HORA (vty_id_usuario, de.fecha, 'D') <= (F_VALOR_HORA_MINIMO (vfe_fecha) * 2) THEN
                            -- Se calcula el valor x hora del subsidio, es decir, se toma el valor de ley del subsidio y 
                            -- se divide en 240 para obtener el valor x hora y ese valor se multiplica x las horas resultantes 
                            -- del prorrateo para cada proyecto
                            vnu_cantidad_dias_subsidio := vnu_cantidad_dias_subsidio + 1;
                        END IF;
                    END LOOP;
                    UPDATE jc_corte_usuarios set porcentaje_prorata = v_porcentaje_prorata,
                                                 subsidio_transporte = 0
                     WHERE id_corte = p_id_corte
                       AND id_usuario = vty_id_usuario;
 
                    -- Resta de los días para realizar calculo de transporte.
                    IF (trunc(vro_cortes.fecha_final) - trunc(vro_cortes.fecha_inicial) + 1 - vnu_dias_festivos) <= vnu_cantidad_dias_subsidio THEN
                        vnu_cantidad_dias_subsidio := 15;
                    END IF;
 
                    -- El valor del subsidio se debe distribuir en cada obra de acuerdo a la cantidad de horas trabajadas.
                    -- total de horas trabajadas en el mes vnu_sum_cantidad_horas, v_cantidad_horas_nocturnas.
                    FOR oc IN cur_sum_usuario_obra_corte LOOP
                        vnu_subsidio := null;
                        IF (NVL(vnu_sum_cantidad_horas,0) + NVL(v_cantidad_horas_nocturnas,0)) <> 0 THEN
                        vnu_subsidio := ((v_subsidio_mens_transporte / 30) * vnu_cantidad_dias_subsidio) * 
                                        ((oc.cantidad_horas + oc.cantidad_horas_nocturnas) /(NVL(vnu_sum_cantidad_horas,0) + NVL(v_cantidad_horas_nocturnas,0)));
                        ELSE
                                insert  into log_mensajes values (sysdate, '2. Usuario: '||vty_id_usuario);
                        END IF;
                        UPDATE jc_corte_usuarios set subsidio_transporte = vnu_subsidio, DIAS_SUBSIDIO_TRANSPORTE = vnu_cantidad_dias_subsidio
                         WHERE id_corte = p_id_corte
                           AND id_obra = oc.id_obra
                           AND id_usuario = vty_id_usuario;
                    END LOOP;                
 
                    -- v_subsidio_mens_transporte
                    --UPDATE jc_corte_usuarios set porcentaje_prorata = v_porcentaje_prorata,
                    --                             subsidio_transporte = vnu_ref_subsidio_transporte
                    -- WHERE id_corte = p_id_corte
                    --   AND id_usuario = vty_id_usuario;
 
                    vro_acum_corte_usuarios.id_usuario          := vty_id_usuario;
                    vro_acum_corte_usuarios.id_corte            := p_id_corte;
 
                    OPEN cur_usuario_nov;
                    FETCH cur_usuario_nov INTO vty_id_usuario_n;
                    IF cur_usuario_nov%NOTFOUND THEN
                        vbo_existe := TRUE;
                        -- El cursor puede pasar mas de una vez si tiene varias obras y
                        -- debe acumular una sola vez
                        OPEN cur_acum_corte_usuario;
                        FETCH cur_acum_corte_usuario INTO vro_acum_cu;
                        IF cur_acum_corte_usuario%NOTFOUND THEN
                            vbo_existe := FALSE;
                        END IF;
                        CLOSE cur_acum_corte_usuario;
 
                        IF vbo_existe = FALSE THEN
                            vbo_existe_corte_anterior := FALSE;
                            OPEN cur_corte_anterior;
                            FETCH cur_corte_anterior INTO vro_jc_corte_anterior;
                            IF cur_corte_anterior%FOUND THEN
                                vbo_existe_corte_anterior := TRUE;
                            END IF;
                            CLOSE cur_corte_anterior;
 
                            IF v_compensa_total is not null or vnu_tiempo_autorizado is not null or vnu_tiempo_pendiente is not null THEN
                                --vbo_existe := TRUE;
                                IF vbo_existe = FALSE THEN
                                    vro_acum_corte_usuarios.id                  := jc_acum_corte_usuarios_seq.nextval;
                                    vro_acum_corte_usuarios.id_corte            := p_id_corte;
 
                                    vro_acum_corte_usuarios.tiempo_autorizado   := vnu_tiempo_autorizado;
                                    vro_acum_corte_usuarios.tiempo_pendiente    := vnu_tiempo_pendiente;
                                    IF vbo_existe_corte_anterior = TRUE THEN
                                        vnu_id_corte := vro_jc_corte_anterior.id;
                                        OPEN cur_acum_corte_usuario;
                                        FETCH cur_acum_corte_usuario INTO vro_acum_corte_usuarios_ant;
                                        IF cur_acum_corte_usuario%FOUND THEN
                                            vro_acum_corte_usuarios.tiempo_acumulado_compensar := vro_acum_corte_usuarios_ant.tiempo_acumulado_compensar - 
                                                                                                  NVL(v_compensa_total,0);
                                        ELSE
                                            vro_acum_corte_usuarios.tiempo_acumulado_compensar := NVL(v_compensa_total,0);
                                        END IF;
                                        CLOSE cur_acum_corte_usuario;
                                    ELSE
                                        vro_acum_corte_usuarios.tiempo_acumulado_compensar := NVL(v_compensa_total,0);
                                    END IF;
/*IF vro_acum_corte_usuarios.id_usuario = 171 THEN
    --dbms_output.put_line('2. ID: '||vty_id_usuario||' vro_acum_corte_usuarios_id_usuario: '||vro_acum_corte_usuarios.id_usuario||' Tiempo autorizado: '||vnu_tiempo_autorizado|| ' ' ||' Tiempo Pendiente: '||vnu_tiempo_pendiente);
END IF;    */
                                    
                                    insert into jc_acum_corte_usuarios values vro_acum_corte_usuarios;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                    CLOSE cur_usuario_nov;
 
                END LOOP;
 
                -- Ciclo para los usuarios que no tiene obra.  en estudio xq a partir de aqui se descontrola - jul29-2024
               FOR so IN cur_usuarios_sin_obra LOOP
                    vty_id_usuario := so.id_usuario;
 
                    OPEN cur_sum_tiempo_compensado;
                    FETCH cur_sum_tiempo_compensado INTO v_compensa_extras, v_compensa_extra_noche, v_compensa_festivo, v_compensa_total;
                    CLOSE cur_sum_tiempo_compensado;
 
                    OPEN cur_sum_tiempo_autoriza;
                    FETCH cur_sum_tiempo_autoriza INTO vnu_tiempo_autorizado, vnu_tiempo_pendiente;
                    CLOSE cur_sum_tiempo_autoriza;
 
                    OPEN cur_usuario_nov;
                    FETCH cur_usuario_nov INTO vty_id_usuario_n;
                    IF cur_usuario_nov%NOTFOUND THEN
                        vbo_existe := TRUE;
                        -- El cursor puede pasar mas de una vez si tiene varias obras y
                        -- debe acumular una sola vez
                        OPEN cur_acum_corte_usuario;
                        FETCH cur_acum_corte_usuario INTO vro_acum_cu;
                        IF cur_acum_corte_usuario%NOTFOUND THEN
                            vbo_existe := FALSE;
                        END IF;
                        CLOSE cur_acum_corte_usuario;
 
                        IF vbo_existe = FALSE THEN
                            vbo_existe_corte_anterior := FALSE;
                            OPEN cur_corte_anterior;
                            FETCH cur_corte_anterior INTO vro_jc_corte_anterior;
                            IF cur_corte_anterior%FOUND THEN
                                vbo_existe_corte_anterior := TRUE;
                            END IF;
                            CLOSE cur_corte_anterior;
 
                            IF v_compensa_total is not null or vnu_tiempo_autorizado is not null or vnu_tiempo_pendiente is not null THEN
                                --vbo_existe := TRUE;
                                IF vbo_existe = FALSE THEN
                                    vro_acum_corte_usuarios.id                  := jc_acum_corte_usuarios_seq.nextval;
                                    vro_acum_corte_usuarios.id_corte            := p_id_corte;
                                    vro_acum_corte_usuarios.id_usuario          := vty_id_usuario;
                                    vro_acum_corte_usuarios.tiempo_compensar    := null;
                                    vro_acum_corte_usuarios.tiempo_autorizado   := vnu_tiempo_autorizado;
                                    vro_acum_corte_usuarios.tiempo_pendiente    := vnu_tiempo_pendiente;
                                    IF vbo_existe_corte_anterior = TRUE THEN
                                        vnu_id_corte := vro_jc_corte_anterior.id;
                                        OPEN cur_acum_corte_usuario;
                                        FETCH cur_acum_corte_usuario INTO vro_acum_corte_usuarios_ant;
                                        IF cur_acum_corte_usuario%FOUND THEN
                                            vro_acum_corte_usuarios.tiempo_acumulado_compensar := vro_acum_corte_usuarios_ant.tiempo_acumulado_compensar - 
                                                                                                  NVL(v_compensa_total,0);
                                        ELSE
                                            vro_acum_corte_usuarios.tiempo_acumulado_compensar := NVL(v_compensa_total,0);
                                        END IF;
                                        CLOSE cur_acum_corte_usuario;
                                    ELSE
                                        vro_acum_corte_usuarios.tiempo_acumulado_compensar := NVL(v_compensa_total,0);
                                    END IF;
--F vro_acum_corte_usuarios.id_usuario = 171 THEN
    --dbms_output.put_line('3. ID: '||vty_id_usuario||' vro_acum_corte_usuarios_id_usuario: '||vro_acum_corte_usuarios.id_usuario||' Tiempo autorizado: '||vnu_tiempo_autorizado|| ' ' ||' Tiempo Pendiente: '||vnu_tiempo_pendiente);
--END IF;    
                                    
                                    insert into jc_acum_corte_usuarios values vro_acum_corte_usuarios;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                    CLOSE cur_usuario_nov;
 
                END LOOP;
            END IF;
            COMMIT;
        END IF;
        commit;
        --dbms_output.put_line('Generando Plano');
        --P_GENERAR_PLANO_NOMINA(p_id_corte, v_codigo, v_mensaje);
        P_GENERAR_ARCHIVO_NOMINA(p_id_corte, v_codigo, v_mensaje);
        vro_cortes.cantidad_liquidaciones := NVL(vro_cortes.cantidad_liquidaciones,0) + 1;
        update jc_cortes set cantidad_liquidaciones = vro_cortes.cantidad_liquidaciones 
         where id = vro_cortes.id;
        commit;
    END IF;
END P_GENERAR_LIQUIDACION;