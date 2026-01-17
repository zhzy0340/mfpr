% 读取 Excel 文件
df1 = readtable('/Users/yunya/Documents/reho/ckg1-r2.xlsx');
df2 = readtable('/Users/yunya/Documents/reho/ckg1-r1.xlsx');

% 合并两个表格，基于 'ID' 和 'Session'
merged_df = outerjoin(df1, df2, 'Keys', {'ID', 'Session'}, 'MergeKeys', true);

% 获取所有列名
cols = merged_df.Properties.VariableNames;

% 对每一个 'Brain_Area' 列进行处理
for i = 1:length(cols)
    if contains(cols{i}, 'ID') || contains(cols{i}, 'Session')
        continue;  % 跳过 'ID', 'Session' 列
    end
    
    % 生成 '_r2' 和 '_r1' 对应的列名
    col_r2 = strcat(cols{i}, '_r2');
    col_r1 = strcat(cols{i}, '_r1');
    
    % 如果 'r1' 和 'r2' 的列都存在，进行行平均
    if ismember(col_r2, merged_df.Properties.VariableNames) && ismember(col_r1, merged_df.Properties.VariableNames)
        % 对两个列进行平均，处理 NaN（如果存在 NaN 则用另一个非 NaN 的值）
        merged_df.(cols{i}) = (merged_df.(col_r2) + merged_df.(col_r1)) / 2;
    elseif ismember(col_r2, merged_df.Properties.VariableNames)
        % 如果只有 r2 存在，保留 r2 列的值
        merged_df.(cols{i}) = merged_df.(col_r2);
    elseif ismember(col_r1, merged_df.Properties.VariableNames)
        % 如果只有 r1 存在，保留 r1 列的值
        merged_df.(cols{i}) = merged_df.(col_r1);
    end
end

% 删除临时生成的 '_r1' 和 '_r2' 列
vars_to_remove = {};
for i = 1:length(cols)
    if contains(cols{i}, '_r1') || contains(cols{i}, '_r2')
        vars_to_remove{end+1} = cols{i}; %#ok<AGROW>
    end
end

% 删除列
merged_df = removevars(merged_df, vars_to_remove);

% 保存结果到一个新的 Excel 文件
writetable(merged_df, '/Users/yunya/Documents/reho/matched_result.xlsx');

% 输出匹配结果统计
num_matched = sum(~any(ismissing(merged_df{:, 3:end}), 2));  % 匹配的行数
num_unmatched = height(merged_df) - num_matched;  % 未匹配的行数
disp(['Matched rows: ', num2str(num_matched)]);
disp(['Unmatched rows: ', num2str(num_unmatched)]);